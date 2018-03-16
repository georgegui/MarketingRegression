#' @import data.table
#' @import CommonFunctions
#' @import bayesm
NULL


EstimateRegion.Bayes <- function(m, dt){
  lhs     <- 'sales'
  rhs_pat <- '^price_[0-9]|^promotion_[0-9]|^promotion_interaction_[0-9]'
  unique_stores <- unique(dt[, store_code_uc])
  rhs_cols      <- grep(rhs_pat, names(dt), value = TRUE)
  regdata       <- lapply(unique_stores, function(x)
    DcastToBayes(dt[store_code_uc == x], lhs, rhs_cols))
  data1         <- list(regdata = regdata)
  mcmc1         <- list(R = m$n_draws, keep = m$thickness, nprint = 5000)
  out           <- NULL
  if(IsVerboseMode()){
    out <- rhierLinearModel(Data = data1, Mcmc = mcmc1)
  } else {
    capture.output(out <- rhierLinearModel(Data = data1, Mcmc = mcmc1))
  }
  rhs_var_list  <- unlist(lapply(regdata, function(x) colnames(x$X)))
  store_list    <- unlist(lapply(unique_stores,
    function(x) rep(x, length(rhs_cols))))
  delta <- SummaryDelta(out$Deltadraw, rhs_cols)
  out <- SummaryBeta(out$betadraw, store_list, rhs_var_list)
  attr(out, 'delta') <- delta
  return(out)
}

#' @describeIn Estimate Bayes
#' @export
Estimate.Bayes <- function(m, move){
  results <- list()
  for(lhs_group in m$lhs_groups){
    competing_groups <- SelectCompetingProducts(lhs_group, m)
    rhs_variables    <- SelectRhsVariables(m)
    valid_variables  <- SelectControls(m, lhs_group, competing_groups)
    valid_variables  <- c('week_end', 'store_code_uc', valid_variables)
    dt <- move[group_code %in% competing_groups]
    dt <- dcast(dt,
      week_end + store_code_uc ~ group_code,
      value.var = rhs_variables, fill = 0)
    dt <- dt[, .SD, .SDcols = intersect(names(dt), valid_variables)]
    lhs_cols <- c('sales', 'store_code_uc', 'week_end', m$estimate_by_group)
    lhs_sales <- move[lhs_group == group_code,
                      intersect(lhs_cols, names(move)), with = FALSE]
    dt        <- merge(lhs_sales, dt, by = c('week_end', 'store_code_uc'))
    n_store <- CountUnique(dt[, store_code_uc])
    VerboseWarning('Processing', nrow(dt), 'dcasted table for group ', lhs_group)
    if(nrow(dt) == 0) next
    if(m$estimate_by_group == 'National'){
      out <- EstimateRegion.Bayes(m, dt)
    } else {
      dt[, grp_id := .GRP, by = c(m$estimate_by_group)]
      out_list <- list()
      delta_list <- list()
      for(i in unique(dt$grp_id)){
        cur_dt <- dt[grp_id == i]
        cur_out <- EstimateRegion.Bayes(m, cur_dt)
        cur_group <- cur_dt[, .SD[1], .SDcols = c(m$estimate_by_group)]
        out_list[[i]] <- cbind(cur_out, cur_group)
        delta_list[[i]] <- cbind(attr(cur_out, 'delta'), cur_group)
      }
      # out <- dt[, EstimateRegion.Bayes(m, .SD), by = c(m$estimate_by_group)]
      out <- rbindlist(out_list)
    }
    out_summary <- ModelSpecificSummary(m, out, n_store, lhs_group = lhs_group,
                                        delta = rbindlist(delta_list))
    results[[lhs_group]] <- out_summary
  }
  return(results)
}


# Dcast a data.table to a list of matrix to be ready to be put into
# hierarchical bayes regression in bayesm
DcastToBayes <- function(DT, LHS, RHS){
  # this function convert a data.table to a vector y and a matrix X so it satisfies
  # the data format of bayesm::hierLinearModel
  list(y = DT[, get(LHS)], X = as.matrix(DT[, .SD, .SDcols = RHS]))
}

# Get Coefficient of a Regression
GetCoef <- function(L){
  # this function get the coefficient of a linear regression w/o intercept
  coef(lm(L$y ~ L$X - 1))
}

# Produce summary on the first level prior
#' @param X object$deltadraw
SummaryDelta <-  function(X, names, burnin = NULL, tvalues = NULL,
                          QUANTILES = TRUE, TRAILER = TRUE, ...) {
  # this function summarizes the delta of a bayesm matrix object
  if(is.null(burnin)) burnin = trunc(0.1 * nrow(X))
  X <- X[(burnin + 1):nrow(X), , drop = FALSE]
  mat <- matrix(apply(X, 2, mean), nrow = 1)
  quantile_list <- c(0.005, seq(0.025, 0.975, 0.025), 0.995)
  quantile_names <- paste0('q', sprintf("%03d",quantile_list * 1e3))
  qmat <- apply(X, 2, quantile, probs = quantile_list)
  mat <- t(rbind(mat,qmat))
  rownames(mat) <- names

  colnames(mat) <- c('mean', quantile_names)
  mat <- data.table(rhs_var = rownames(mat), mat)
  return(mat)
}

# Summarize the posterior mean of the beta coefficients
SummaryBeta <- function(object, store_list, rhs_var_list){
  # this function summarizes the beta of a rhierLinearModel result
  X           <- object
  cur_dim     <- dim(X)
  ndraw       <- cur_dim[[3]]
  nreg        <- cur_dim[[1]]
  result.mean <- list()
  result_quantile <- list()
  quantile_list <- c(0.005, seq(0.025, 0.975, 0.025), 0.995)
  for (reg in 1:nreg){
    result.mean[[reg]] <- rowMeans(X[reg, , (0.1 * ndraw): ndraw])
    result_quantile[[reg]] <- t(apply(X[reg, ,(0.1 * ndraw): ndraw], 1,
      quantile, probs = quantile_list))
  }
  result_quantile <- lapply(result_quantile, data.table)
  result_quantile <- rbindlist(result_quantile)
  quantile_names <- paste0('q', sprintf("%03d",quantile_list * 1e3))
  setnames(result_quantile, names(result_quantile), quantile_names)
  result  <- data.table(
    post_mean     = unlist(result.mean),
    rhs_var       = rhs_var_list,
    store_code_uc = store_list,
    result_quantile
  )
  return(result)
}




#' #' Fill in the ZIP3 of a data.table
#' FillZip3 <- function(DT){
#'   unique_groups <- unique(DT$group_code)
#'   unique_stores <- unique(DT$store_code_uc)
#'   unique_weeks  <- unique(DT$week_end)
#'   DT_ID         <- data.table(ID = levels(interaction(unique_groups, unique_stores, unique_weeks, sep = '_')))
#'   return(DT_ID)
#' }

