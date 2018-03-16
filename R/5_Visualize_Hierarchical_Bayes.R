#' @import data.table
#' @import CommonFunctions
#' @import bayesm
NULL

# this function draws the MCMC of the Bayes Estimation
DrawBayesMcmc <- function(move, lhs_variable_name = 'log_volume_sold',
                          rhs_variable_name = c('log_imputed_price_hybrid', 'promotion_freq_5'),
                          all_promotion = TRUE, select_top = FALSE, lhs_groups_list = unique(move$group_code), lhs_groups_name = unique(move$group_code),
                          rhs_groups_list = NULL, graph_folder = NULL, error_file = '~/BayesHier/Log_Folder/error.log'){
  unique_groups   <- unique(move$group_code)
  unique_stores   <- unique(move$store_code_uc)
  lhs_groups_list <- intersect(lhs_groups_list, unique_groups)
  rhs_groups_list <- intersect(rhs_groups_list, unique_groups)
  move_dcast_rhs  <- dcast(move, week_end + store_code_uc ~ group_code, value.var = rhs_variable_name, fill = 0)
  move_dcast_lhs  <- dcast(move, week_end + store_code_uc ~ group_code, value.var = c(lhs_variable_name), fill = NA)

  lhs_variable_list <- paste0('volume_sold_', unique_groups)

  rhs_variable_list <- setdiff(names(move_dcast_rhs), c('week_end', 'store_code_uc'))
  # rhs_variable_list <- rhs_variable_list[order(rhs_variable_list)]
  setnames(move_dcast_lhs, as.character(unique_groups), lhs_variable_list)
  move_dcast        <- merge(move_dcast_lhs, move_dcast_rhs, by = c('week_end', 'store_code_uc'))
  result_list       <- list()
  count             <- 0
  rhs_formula       <- do.call(paste, list(rhs_variable_list, collapse = ' + '))
  for(group_id in 1:length(lhs_groups_list)){
    lhs_group       <- lhs_groups_list[[group_id]]
    lhs_group_name  <- lhs_groups_name[[group_id]]
    cur_folder_path <- paste0(graph_folder, lhs_group_name, '/')
    if(!file.exists(cur_folder_path)) dir.create(cur_folder_path)
    lhs_group_col <- paste0('volume_sold_', lhs_group)
    count         <- count + 1
    DT            <- move_dcast[!is.na(get(lhs_group_col))]
    if(select_top){
      lhs_index <- which(lhs_group==lhs_groups_list)
      if(length(lhs_index)==0){
        next
      }
      if(lhs_group %in% rhs_groups_list[1:min(5, length(rhs_groups_list))]){
        competing_groups <- rhs_groups_list[1:min(5, length(rhs_groups_list))]
      } else {
        competing_groups <- union(rhs_groups_list[1:min(4, length(rhs_groups_list))], lhs_group)
      }
    } else {
      competing_groups <- unique_groups
    }

    cur_rhs_variable_list <- paste0('log_imputed_price_hybrid_', competing_groups)
    if(all_promotion){
      cur_rhs_variable_list <- c(cur_rhs_variable_list, paste0('promotion_freq_5_', competing_groups))
    } else {
      cur_rhs_variable_list <- c(cur_rhs_variable_list, paste0('promotion_freq_5_', lhs_group))
    }
    regdata      <- lapply(unique_stores, function(x) DcastToBayes(DT[store_code_uc == x], lhs_group_col, cur_rhs_variable_list))
    rhs_var_list <- unlist(lapply(regdata, function(x) colnames(x$X)))
    store_list   <- unlist(lapply(unique_stores, function(x) rep(x, length(cur_rhs_variable_list))))
    rhs_var_list <- paste(rhs_var_list, store_list, sep = '_')
    # suppressWarnings(rhs_var_list <- paste0(levels(interaction(rhs_variable_list, unique_stores, sep = '_'))))
    lhs_var_list <- rep(lhs_group_col, length(rhs_var_list))
    data1        <- list(regdata = regdata)
    mcmc1        <- list(R = 40000, keep = 1, nprint = 5000)
    out <- NULL
    tryCatch(out <- rhierLinearModel.noprint(Data = data1, Mcmc = mcmc1),
             error = function(e){
               error_line = paste(lhs_group, min(move$stores))
               write(error_line, file = error_file, append = TRUE)
             })
    if(is.null(out)){
      next
    }
    plot_var_names <- sub(lhs_group, 'own', cur_rhs_variable_list)
    for(cur_R in c(2000, 5000, 15000, 40000)){
      cur_path    <- paste0(cur_folder_path, cur_R, 'draw.pdf')
      pdf(cur_path, onefile = TRUE)
      plot.bayesm.coef.fixed(out$betadraw[, , 1:cur_R], names = plot_var_names)
      dev.off()
    }
    cur_path    <- paste0(cur_folder_path,'MCMC.pdf')
    plot_index = seq(1,40000,10)
    pdf(cur_path, onefile = TRUE)
    for(col_id in 1:length(plot_var_names)){
      plot(plot_index, out$Deltadraw[plot_index, col_id], main = plot_var_names[[col_id]])
    }
    dev.off()
    #result_list[[count]] <- data.table(summary.bayesm.mat.new(out$Deltadraw, names = rhs_variable_list))
  }
}

#------------------------------------------------------------------------------
plot.bayesm.coef.fixed <- function (x, names, burnin = trunc(0.1 * R), ...)
{
  # this function changes the setting of plot.bayesm.coef function:
  # 1. it disables the "return to show next plot" so that the code can run
  # without interupttion
  # 2. it disables the last plot showing the distribution of the coefficient
  # 3. rather than generating random sample of unit, this function generates
  #   a fixed list of samples
  X = x
  if (mode(X) == "list")
    stop("list entered \n Possible Fixup: extract from list \n")
  if (mode(X) != "numeric")
    stop("Requires numeric argument \n")
  d = dim(X)
  if (length(d) != 3)
    stop("Requires 3-dim array \n")
  op = par(no.readonly = TRUE)
  on.exit(par(op))
  on.exit(devAskNewPage(FALSE), add = TRUE)
  nunits = d[1]
  nvar = d[2]
  R = d[3]
  if (missing(names)) {
    names = as.character(1:nvar)
  }
  if (R < 100) {
    cat("fewer than 100 draws submitted \n")
    return(invisible())
  }
  rsam = sort(sample(c(1:nunits), 30))
  rsam = seq(1, nunits, max(1,floor(nunits/30)))
  par(mfrow = c(1, 1))
  par(las = 3)
  devAskNewPage(FALSE)
  for (var in 1:nvar) {
    ext = X[rsam, var, (burnin + 1):R]
    ext = data.frame(t(ext))
    colnames(ext) = as.character(rsam)
    out = boxplot(ext, plot = FALSE, ...)
    out$stats = apply(ext, 2, quantile, probs = c(0, 0.05,
                                                  0.95, 1))
    bxp(out, xlab = "Cross-sectional Unit", main = paste("Coefficients on Var ",
                                                         names[var], sep = ""), boxfill = "magenta", ...)
  }
  par(las = 1)
  pmeans = matrix(0, nrow = nunits, ncol = nvar)
  for (i in 1:nunits) pmeans[i, ] = apply(X[i, , (burnin +
                                                    1):R], 1, mean)
  attributes(pmeans)$class = "bayesm.mat"
  for (var in 1:nvar) names[var] = paste("Posterior Means of Coef ",
                                         names[var], sep = "")
  # plot(pmeans, names, TRACEPLOT = FALSE, INT = FALSE, DEN = FALSE,
  #     CHECK_NDRAWS = FALSE, ...)
  invisible()
}
environment(plot.bayesm.coef.fixed) <- asNamespace('bayesm')
