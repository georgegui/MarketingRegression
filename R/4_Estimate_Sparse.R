#' @import Matrix
#' @import data.table
#' @import CommonFunctions
#' @import MatrixModels
NULL


# A framework function for Sparse Regression
#
# this function is a framework for Sparse Regression that can be applied to
#   regressions like SparseOLS or SparseLASSO

Estimate.Sparse <- function(m, move, func, ...){
  results <- list()
  old_key <- key(move)
  setkey(move, week_end, store_code_uc)
  for(lhs_group in m$lhs_groups){
    lhs_dt <- move[, list(includeLhs = lhs_group %in% group_code),
                   by = .(week_end, store_code_uc)]
    dt <- merge(move, lhs_dt[includeLhs == TRUE])
    n_store <- CountUnique(dt[, store_code_uc])
    if(nrow(dt) == 0) next
    if(m$regional_control == 'National'){
      out <- EstimateRegion.Sparse(m, dt, lhs_group, func, ...)
    } else {
      out <- dt[, EstimateRegion.Sparse(m, .SD, lhs_group, func, ...),
                by = c(m$regional_control)]
    }
    out_summary <- ModelSpecificSummary(m, out, n_store, lhs_group = lhs_group)
    results[[lhs_group]] <- out_summary
  }
  setkeyv(move, old_key)
  return(results)
}


# This function is a framework for Estimation of a certain region
EstimateRegion.Sparse <- function(m, dt, lhs_group, func, ...){
  dt_row <- dt[, list(row_index = as.integer(.GRP)),
               by = .(week_end, store_code_uc)]
  dt_rhs <- ConstructSparseRhs(m, dt, lhs_group, dt_row)
  if(nrow(dt_rhs) < ncol(dt_rhs)) return(NULL)
  dt_lhs <- merge(dt[lhs_group == group_code], dt_row,
                  by = c('week_end', 'store_code_uc'))
  dt_lhs <- dt_lhs[order(row_index), sales]
  VerboseWarning(nrow(dt_rhs), 'rows data for group', lhs_group)
  out <- NULL
  tryCatch(out <- func(dt_rhs, dt_lhs, ...),
           error = function(e){
             cat('Error for group', lhs_group, 'using model', m$model_name,
                 'for stores like', min(dt[, store_code_uc]),  '\n')
           })
  return(out)
}


#' @export
ConstructSparseRhs <- function(m, dt, lhs_group, dt_row){
  # melt tables
  rhs_variables    <- SelectRhsVariables(m)
  competing_groups <- SelectCompetingProducts(lhs_group, m)
  id_vars <- c('store_code_uc',
               'group_code',
               'week_end',
               m$time_control)
  dt <- melt(dt[group_code %in% competing_groups],
             id.vars = id_vars,
             measure.vars = rhs_variables)
  dt <- merge(dt, dt_row, by = c('week_end', 'store_code_uc'))
  # select valid controls
  valid_controls <- SelectControls(m, lhs_group, competing_groups)
  dt[, variable_name := paste(variable, group_code, sep = '_')]
  dt <- dt[variable_name %in% valid_controls]
  # generate brand controls
  dt <- dt[, count := CountUnique(value),
           by = .(variable, group_code, store_code_uc)
           ][count > 1]
  dt[, column_index := as.integer(.GRP),
     by = .(variable, group_code, store_code_uc)]
  dt_rhs <- sparseMatrix(dt[, row_index],
                         dt[, column_index],
                         x = dt[, value])
  setkey(dt, column_index)
  column_table <- unique(
    dt[, .(column_index, variable, group_code, store_code_uc)])
  # generate store controls
  dummy_col <- rep(1, nrow(dt))
  dt[, store_index := as.integer(.GRP), by = store_code_uc]
  dt_store <- sparseMatrix(dt[, row_index],
                           dt[, store_index],
                           x = dummy_col) > 0 # to factor of 1 or 0
  n_store_cols <- ncol(dt_store)
  # generate month controls
  if(!m$demean_before_estimation & (!is.null(m$time_control))){
    dt[, month_index := as.integer(.GRP), by = c(m$time_control)]

    #-1 denote deleting the last column of the month index
    dt_month <- sparseMatrix(dt[, row_index],
                             dt[, month_index],
                             x = dummy_col)[, -1] > 0

    dt_month <- dt_month[, colSums(dt_month)>1]
    dt <- cbind(dt_rhs, dt_month, dt_store)
    n_month_cols <- ncol(dt_month)
  } else {
    dt <- cbind(dt_rhs, dt_store)
    n_month_cols <- 0
  }
  attr(dt, 'relevant_cols') <- column_table
  attr(dt, 'n_month_cols') <- n_month_cols
  attr(dt, 'n_store_cols') <- n_store_cols

  return(dt)
}
