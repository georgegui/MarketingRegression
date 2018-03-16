#' @import data.table
#' @import CommonFunctions
NULL

EstimateRegion.OLS <- function(dt, store_code_uc){
  cols_to_keep <- lapply(dt, CountUnique) > 1
  cols_to_keep <- names(cols_to_keep[cols_to_keep == TRUE])
  rhs_pat <- '^(price|promotion|promotion_interaction)(|_[0-9])$'
  rhs_cols <- grep(rhs_pat, cols_to_keep, value = TRUE)
  cur_formula <- formula(paste0('sales~', paste(rhs_cols, collapse = '+')))
  out <- summary(lm(cur_formula, dt))$coefficient[-1, ]
  data.table(store_code_uc = store_code_uc,
             rhs_var = rhs_cols,
             coefficient = out[, 1],
             pvalue = out[, 4])
}

#' @describeIn Estimate the Basic OLS regression.
#'
#' It is used as a benchmark for testing the correctness of the model. However,
#' when the FWL theorem is not used properly, this estimation will not return
#' the correct results.
#'@export
Estimate.OLS <- function(m, move){
  results <- list()
  old_key <- key(move)
  setkey(move, week_end, store_code_uc)
  for(lhs_group in m$lhs_groups){
    rhs_variables    <- SelectRhsVariables(m)
    competing_groups <- SelectCompetingProducts(lhs_group, m)
    lhs_dt <- move[, list(includeLhs = lhs_group %in% group_code),
                   by = .(week_end, store_code_uc)]
    dt <- merge(move, lhs_dt[includeLhs == TRUE])
    if(nrow(dt) == 0) next
    dt <- dt[group_code %in% competing_groups]
    dt <- dcast(dt,
                week_end + store_code_uc ~ group_code,
                value.var = rhs_variables, fill = NA)
    valid_variables  <- SelectControls(m, lhs_group, competing_groups)
    valid_variables  <- c('week_end', 'store_code_uc', valid_variables)
    dt <- dt[, .SD, .SDcols = intersect(names(dt), valid_variables)]
    lhs_sales <- move[lhs_group == group_code,
                      .(sales, store_code_uc, week_end)]
    dt <- merge(lhs_sales, dt, by = c('week_end', 'store_code_uc'))
    n_store <- CountUnique(dt[, store_code_uc])
    out <- dt[, EstimateRegion.OLS(.SD, store_code_uc), by = store_code_uc]
    out_summary <- ModelSpecificSummary(m, out, n_store, lhs_group = lhs_group)
    results[[lhs_group]] <- out_summary
  }
  setkeyv(move, old_key)
  return(results)
}

