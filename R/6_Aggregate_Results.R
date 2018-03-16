#' @import data.table
NULL



#' Save small files into big file for efficieny
#'
#' This fucntion will take in all result files of a model. It will save them
#' by 'var_name'(price, promotion) and 'rhs_group'(own, cross, ...)
#'
#' @export
#' @param cur_model the name of the model
#' @param var_keys by what keys the result should be saved
#' @param variable_to_summarize a data.table where each row is the key values to
#'  be saved.
#' @param n_cores the number of cores to be used if in ParallelMode. Too many
#'  cores will result in lost in efficieny. However, too few cores will result
#'  into error because the data that each core pass back to the main process
#'  will exceed the memory limit
#'
SaveAggregatedResults <- function(cur_model, variable_to_summarize,
                                  n_cores = 8,
                                  var_keys = c('var_name', 'rhs_group'),
                                  keep_model_name = TRUE,
                                  map_group_code = FALSE){
  file_names <- levels(interaction(module_list, cur_model, sep = '_'))
  file_names <- paste0(PATHS$output$intermediary$dir, file_names, '.RData')
  load_func <- function(x){
    dt <- LoadData(x, 'results')
    if(is.null(dt)) return(NULL)
    dt <- lapply(dt, FormatResult, map_group_code = map_group_code)
    if(!keep_model_name){
      dt <- lapply(dt, function(x) x[, model_name := NULL])
    }
    dt <- lapply(dt, function(x) setkeyv(x, var_keys))
    return(dt)
  }
  ExtractFunc <- function(dt, key_dt){
    dt <- dt[key_dt]
    dt[, .SD, .SDcols = setdiff(names(dt), names(key_dt))]
  }
  all_results <- ParallelApply(file_names, load_func, n_cores = n_cores)
  all_results <- unlist(all_results, recursive = FALSE)
  for(var_id in 1:nrow(variables_to_summarize)){
    key_dt <- variables_to_summarize[var_id]
    cur_var <- variables_to_summarize[var_id, var_name]
    cur_rhs_group <- variables_to_summarize[var_id, rhs_group]
    save_file_path <- paste('Agg', cur_model, cur_rhs_group, cur_var, sep = '_')
    save_file_path <- paste0(PATHS$output$intermediate_summary$dir,
                             save_file_path, '.RData')
    if(file.exists(save_file_path)){
      next
    }
    results <- lapply(all_results, ExtractFunc, key_dt = key_dt)
    results <- rbindlist(results)
    attr(results, 'rhs_group') <- cur_rhs_group
    attr(results, 'var_name') <- cur_var
    attr(results, 'model_name') <- cur_model
    if(!keep_model_name) results[, model_name := NULL]
    save(results, file = save_file_path)
  }
  return(NULL)
}

#' Updated Version: Save small files into big file for efficieny
#'
#' This fucntion will take in all result files of a model. It will save them
#' by 'var_name'(price, promotion) and 'rhs_group'(own, cross, ...)
#'
#' @export
SaveAggregatedResultsNew <- function(cur_model, variable_to_summarize,
                                  n_cores = 8,
                                  var_keys = c('var_name', 'rhs_group'),
                                  keep_model_name = TRUE,
                                  map_group_code = FALSE){

  file_names <- levels(interaction(module_list, cur_model, sep = '_'))
  file_names <- paste0(PATHS$output$intermediary$dir, file_names, '.RData')
  for(var_id in 1:nrow(variables_to_summarize)){
    key_dt <- variables_to_summarize[var_id]
    cur_var <- variables_to_summarize[var_id, var_name]
    cur_rhs_group <- variables_to_summarize[var_id, rhs_group]
    save_file_path <- paste('Agg', cur_model, cur_rhs_group, cur_var, sep = '_')
    save_file_path <- paste0(PATHS$output$intermediate_summary$dir,
                             save_file_path, '.RData')
    if(file.exists(save_file_path)){
      next
    }
    ExtractFunc <- function(dt, key_dt){
      dt <- dt[key_dt]
      dt[, .SD, .SDcols = setdiff(names(dt), names(key_dt))]
    }
    load_func <- function(x){
      dt <- LoadData(x, 'results')
      if(is.null(dt)) return(NULL)
      dt <- lapply(dt, FormatResult, map_group_code = map_group_code)
      if(!keep_model_name){
        dt <- lapply(dt, function(x) x[, model_name := NULL])
      }
      dt <- lapply(dt, function(x) setkeyv(x, var_keys))
      dt <- lapply(dt, ExtractFunc, key_dt = key_dt)
      return(dt)
    }

    results <- ParallelApply(file_names, load_func, n_cores = n_cores)
    results <- unlist(results, recursive = FALSE)
    results <- rbindlist(results)
    attr(results, 'rhs_group') <- cur_rhs_group
    attr(results, 'var_name') <- cur_var
    attr(results, 'model_name') <- cur_model
    save(results, file = save_file_path)
  }
  return(NULL)
}


#' This function load the aggregated results
#'
#' This function will load the aggregated results saved by SaveAggregatedResults,
#'  of all model for a specific variables.
#' @param var_info the infomration of the variable to be loadded, the default
#'  contains rhs_group and var_name
#' @param file_path the path in which these files are saved.
#' @export
LoadAggregatedResults <- function(var_info, file_path){
  var_pattern  <- paste('Agg_(OLS|LASSO|Bayes).*', var_info$rhs_group, var_info$var_name, sep = '_')
  files_to_load <- grep(var_pattern, dir(file_path), value = TRUE)
  files_to_load <- paste0(file_path, files_to_load)
  results <- ParallelApply(files_to_load, LoadData,
                           dt_name = 'results',
                           n_cores = length(files_to_load))
  results <- results[lapply(results, is.null) == FALSE]
  results <- lapply(results,
                    function(x) x[, model_name := attr(x, 'model_name')])
  results <- rbindlist(results)
  results <- RemoveInfinite(results)
}

#' This function load the aggregated time-window results
#'
#' This function will load the aggregated results saved by SaveAggregatedResults,
#'  of a time window
#'
#' @export
LoadTimeWindowResults <- function(result_path, brand_grouped_basket,
  key_cols = c('YearQuarter', 'State')){
  results <- LoadData(result_path, 'results')
  filter_coefficient = results[, quantile(coefficient, c(0.01, 0.99),
                                          na.rm = TRUE)]
  results <- results[coefficient %between% filter_coefficient]
  # results[, County := tolower(County)]
  results <- merge(results, county_macro_table, by = key_cols)
  brand_grouped_basket[, is_PL := grepl('CTL BR', brand_descr)]
  brand_keys <- c('group_code')
  brand_info_cols <- c('brand_revenue', 'department_descr', 'brand_revenue_rank', 'is_PL', 'module_code') #
  results <- merge(brand_grouped_basket[, .SD, .SDcols = c(brand_keys, brand_info_cols)],
                   results, by = brand_keys)
  results[, weight := (brand_revenue * popestimate)]
  results[, weight := weight/mean(weight, na.rm = TRUE)]
  # results[, YearMonth := as.Date(paste0(YearMonth, '-01'))]
  results[, c('popestimate') := NULL]
  return(results)
}

