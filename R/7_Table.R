
#' Format an exlx column
#'
#' @export
GenerateColumnFormat <- function(dt, wb){
  cell_style_list <- list()
  for(cur_col in names(dt)){
    if(dt[, is.numeric(get(cur_col))]){
      is_integer <- max(dt[, (get(cur_col) + 1e-8)%%1]) < 1e-6
      if(cur_col == 'significant_percentage' & !is_integer){
        print(dt[, get(cur_col)])
      }
      if(is_integer){
        cell_style_list[[cur_col]] <- CellStyle(wb) + DataFormat("#,##0")
      } else {
        cell_style_list[[cur_col]] <- CellStyle(wb) + DataFormat("#,##0.000")
      }

    } else {
      cell_style_list[[cur_col]] <- CellStyle(wb)
    }
  }
  names(cell_style_list) <- 1:length(cell_style_list)
  return(cell_style_list)
}


#' Summarize key quantiles of the results
#'
#' @export
TableSummary <- function(results){
  quantile_list <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99)
  quantile_name <- paste0(quantile_list * 100, '%')
  results_q <- results[, list(quantile_value =
                                wtd.quantile(coefficient, brand_revenue,
                                  quantile_list, normwt = TRUE)),
                       by = model_name]
  results_q[, var_name := quantile_name]
  results_q <- dcast(results_q, model_name ~ var_name, value.var = 'quantile_value')

  results_summary <- results[, list(
    mean = wtd.mean.trim(coefficient, brand_revenue, 0.01),
    median = wtd.quantile(coefficient, brand_revenue, 0.5, normwt = TRUE),
    min = min(coefficient),
    max = max(coefficient),
    n_observation = .N,
    positive_percentage = wtd.mean((coefficient > 0), brand_revenue),
    negative_percentage = wtd.mean((coefficient < 0), brand_revenue),
    less_than_1_percentage = wtd.mean((coefficient < -1), brand_revenue),
    significant_percentage = wtd.mean((significant == 1), brand_revenue)),
    by = model_name]
  results_summary <- merge(results_summary, results_q, by = 'model_name')
  results_summary <- merge(ordered_models, results_summary, by = 'model_name')

  summary_cols <- c('model_id','model_name', 'model_abbrev', 'n_observation',
                    'mean', 'median',
                    'positive_percentage', 'negative_percentage',
                    'less_than_1_percentage', 'significant_percentage',
                    'min', quantile_name, 'max')

  setcolorder(results_summary, summary_cols)
  setkey(results_summary, model_id)
  return(results_summary)
}

wtd.mean.trim <- function(x, w, trim, ...){
  cutoff <- wtd.quantile(x, w, c(trim, 1- trim), normwt = TRUE)
  include_index <- (x < cutoff[[2]]) & (x > cutoff[[1]])
  trimmed_x <- x[include_index]
  trimmed_w <- w[include_index]
  wtd.mean(trimmed_x, trimmed_w, ...)
}
