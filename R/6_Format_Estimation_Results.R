#' @import data.table
#' @import CommonFunctions
NULL

#' Format Estimation Results
#'
#' This function format the estimation results produced by the function
#' Estimate
#' @export
FormatResult <- function(object, ...){
  UseMethod('FormatResult')
}

FormatResultBasic <- function(result, var = 'out'){
  rhs_groups = result$model$rhs_groups
  dt <- copy(result[[var]])
  rank_dt <- data.table(rhs_group = rhs_groups,
                        rank = 1:length(rhs_groups))
  rhs_pattern <- '(.*)_(.*)'
  dt[, var_name    := sub(rhs_pattern, '\\1', rhs_var)]
  dt[, rhs_group   := sub(rhs_pattern, '\\2', rhs_var)]
  dt[, group_code  := result$lhs_group]
  dt <- merge(dt, rank_dt, by = 'rhs_group', all = TRUE)
  dt[rhs_group == group_code, rank := 0]
  dt <- dt[rank <= 3]
  dt[rank == 0, rhs_group := 'own']
  dt[rank > 0, rhs_group := paste('top', rank, 'competing', sep = '_')]
  return(dt)
}

#' @describeIn FormatResult Bayes
#' @export FormatResult.Bayes.Result
FormatResult.Bayes.Result <- function(result,
  brand_cols = c('group_code', 'brand_code_uc_corrected',
    'module_code', 'size1_units'),
  map_group_code = FALSE,
  get_quantile = FALSE){
  dt <- FormatResultBasic(result)
  dt[, significant := q025 * q975 > 0]
  dt[, coefficient := post_mean]
  cols_to_keep <- c('store_code_uc', 'rhs_group', 'var_name', 'group_code',
                    'significant', 'coefficient')
  if(get_quantile){
    col_names <- names(dt)
    q_cols <- col_names[grepl('^q[0-9]', col_names)]
    cols_to_keep <- c(cols_to_keep, q_cols)
    delta <- FormatResultBasic(result, var = 'delta')
  }
  dt <- dt[, cols_to_keep, with = F]
  if(map_group_code & !is.null(result$model$product_info)){
    dt <- merge(dt, result$model$product_info[, .SD, .SDcols = brand_cols],
      by = 'group_code')
  }
  dt <- dt[, model_name := result$model$model_name]
  class(dt) <- c('DemandEstimation.Result', class(dt))
  if(get_quantile) attr(dt, 'delta') <- delta
  return(dt)
}

#' @describeIn FormatResult Sparse OLS
#' @export FormatResult.SparseOLS.Result
FormatResult.SparseOLS.Result <- function(result,
    brand_cols = c('group_code', 'brand_code_uc_corrected',
    'module_code', 'size1_units'),
    map_group_code = FALSE){
  rhs_groups = result$model$rhs_groups
  rank_dt <- data.table(rhs_group = rhs_groups,
                        rank = 1:length(rhs_groups))
  dt <- result$out
  dt[, significant := p_value < 0.05]
  dt[, var_name    := variable]
  dt[, rhs_group   := copy(dt$group_code)]
  dt[, group_code  := result$lhs_group]
  dt <- merge(dt, rank_dt, by = 'rhs_group', all = TRUE)
  dt[rhs_group == group_code, rank := 0]
  dt <- dt[rank <= 3]
  dt[rank == 0, rhs_group := 'own']
  dt[rank > 0, rhs_group := paste('top', rank, 'competing', sep = '_')]
  dt <- dt[, .(store_code_uc, rhs_group, var_name, group_code,
               significant, coefficient)]
  if(map_group_code & !is.null(result$model$product_info)){
    dt <- merge(dt, result$model$product_info[, .SD, .SDcols = brand_cols],
      by = 'group_code')
  }
  dt <- dt[, model_name := result$model$model_name]
  class(dt) <- c('DemandEstimation.Result', class(dt))
  return(dt)
}


#' @describeIn FormatResult Unemployment Result
#' @export FormatResult.TimeWindow.Result
FormatResult.TimeWindow.Result <- function(result,
    brand_cols = c('group_code', 'brand_code_uc_corrected',
    'module_code', 'size1_units'),
    map_group_code = FALSE){
  rhs_groups = result$model$rhs_groups
  rank_dt <- data.table(rhs_group = rhs_groups,
                        rank = 1:length(rhs_groups))
  dt <- result$out
  dt[, significant := p_value < 0.05]
  rhs_pattern <- '(.*)_(.*)'
  dt[, var_name    := sub(rhs_pattern, '\\1', rhs_var)]
  dt[, rhs_group   := sub(rhs_pattern, '\\2', rhs_var)]
  dt[, group_code  := result$lhs_group]
  dt <- merge(dt, rank_dt, by = 'rhs_group', all = TRUE)
  dt[rhs_group == group_code, rank := 0]
  dt <- dt[rank <= 3]
  dt[rank == 0, rhs_group := 'own']
  dt[rank > 0, rhs_group := paste('top', rank, 'competing', sep = '_')]
  cols_to_keep <- c(result$model$regional_control, result$model$extra_cols,
                    'rhs_group', 'var_name', 'group_code',
                    'significant', 'coefficient')
  dt <- dt[, .SD, .SDcols = cols_to_keep]
  if(map_group_code & !is.null(result$model$product_info)){
    dt <- merge(dt, result$model$product_info[, .SD, .SDcols = brand_cols],
      by = 'group_code')
  }
  dt <- dt[, model_name := result$model$model_name]
  class(dt) <- c('DemandEstimation.Result', class(dt))
  return(dt)
}

#' @describeIn FormatResult Sparse LASSO
#' @export FormatResult.SparseLASSO.Result
FormatResult.SparseLASSO.Result <- function(result,
    brand_cols = c('group_code', 'brand_code_uc_corrected',
    'module_code', 'size1_units'),
    map_group_code = FALSE){
  rhs_groups = result$model$rhs_groups
  rank_dt <- data.table(rhs_group = rhs_groups,
                        rank = 1:length(rhs_groups))
  dt <- result$out
  dt[, var_name    := variable]
  dt[, rhs_group   := copy(dt$group_code)]
  dt[, group_code  := result$lhs_group]
  dt <- merge(dt, rank_dt, by = 'rhs_group', all = TRUE)
  dt[rhs_group == group_code, rank := 0]
  dt <- dt[rank <= 3]
  dt[rank == 0, rhs_group := 'own']
  dt[rank > 0, rhs_group := paste('top', rank, 'competing', sep = '_')]
  dt <- dt[, .(store_code_uc, rhs_group, var_name, group_code,
               significant, coefficient)]
  if(map_group_code & !is.null(result$model$product_info)){
    dt <- merge(dt, result$model$product_info[, .SD, .SDcols = brand_cols],
      by = 'group_code')
  }
  dt <- dt[, model_name := result$model$model_name]
  class(dt) <- c('DemandEstimation.Result', class(dt))
  return(dt)
}

#'  SummarizeDemandEstimation the demand estimation results
#'
#' @export
SummarizeDemandEstimation <- function(result){
  mykey <- intersect(c('group_code', 'rhs_group', 'var_name', 'model_name'),
                     names(result))
  tmp <- result[,  list(
    median          = median(coefficient, na.rm = TRUE),
    mean            = mean(coefficient, na.rm = TRUE),
    positivePerc    = mean(coefficient > 0, na.rm = TRUE),
    negativePerc    = mean(coefficient < 0, na.rm = TRUE),
    significantPerc = mean(significant, na.rm = TRUE)),
    by = mykey
    ]
}

GetUniqueKey <- function(object,...){
  UseMethod('GetUniqueKey')
}

GetUniqueKey.list <- function(dt_list, ...){
  is_data_table <- ParallelApply(dt_list, is.data.table)
  if(!all(unlist(is_data_table))){
    stop('Not all elements in the list are data.table.')
  }
  all_keys <- ParallelApply(dt_list, key)
  unique_keys <- unique(all_keys)
  if(length(unique_keys) != 1){
    stop('Not all keys are the same')
  }
  dt <- rbindlist(ParallelApply(
    dt_list, function(x) unique(x)[, .SD, .SDcols = unlist(unique_keys)]))
  dt <- unique(dt)
}

GetUniqueKey.data.table <- function(dt){
  mykey <- key(dt)
  if(is.null(mykey)){
    stop('Keys have not been set')
  }
  unique(dt)[, .SD, .SDcols = mykey]

}

