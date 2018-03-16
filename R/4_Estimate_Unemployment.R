#' @import data.table
#' @import lfe
#' @import CommonFunctions
NULL

EstimateRegion.TimeWindow <- function(dt, m){
  cols_to_keep <- lapply(dt, CountUnique) > 1
  cols_to_keep <- names(cols_to_keep[cols_to_keep == TRUE])
  rhs_pat <- '^(price|promotion|promotion_interaction)(_[0-9]*)$'
  rhs_cols <- grep(rhs_pat, cols_to_keep, value = TRUE)

  for(my_col in rhs_cols){
    n_unique = CountUnique(dt[, get(my_col)])
    if(n_unique == 1) rhs_cols <- setdiff(rhs_cols, my_col)
  }
  if(length(rhs_cols) == 0) return(NULL)
  my_formula <- paste0('sales~', paste(rhs_cols, collapse = '+'))
  if(!m$demean_before_estimation){
    my_formula <- paste0(my_formula, '|store_code_uc')
    if(m$time_control != 'None'){
      my_formula <- paste(my_formula, m$time_control,
                            sep = '+')
    }
  } else {
    my_formula <- paste0(my_formula, '-1')
  }
  my_formula <- formula(my_formula)
  out <- summary(lm(my_formula, dt))$coefficient
  data.table(rhs_var = rhs_cols,
             coefficient = out[, 1],
             p_value = out[, 4])
}

#' @describeIn Estimate the Basic OLS regression of a subset of window.
#'
#' Because we have less data, so we don't try to estimate store-level elasticity
#'
#'@export
Estimate.TimeWindow <- function(m, move){
  window_list <- unique(move$YearQuarter)
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
    meta_cols <- c('store_code_uc', 'week_end',
                           m$regional_control, m$time_control, m$extra_cols)
    dcast_formula <- formula(paste(
      paste(meta_cols, collapse = '+'),
      'group_code', sep = '~'))

    dt <- dcast(dt,
                dcast_formula,
                value.var = rhs_variables, fill = 0)
    valid_variables  <- SelectControls(m, lhs_group, competing_groups)
    valid_variables  <- c(meta_cols, valid_variables)
    dt <- dt[, .SD, .SDcols = intersect(names(dt), valid_variables)]
    lhs_sales <- move[lhs_group == group_code,
                      .(sales, store_code_uc, week_end)]
    dt <- merge(lhs_sales, dt, by = c('week_end', 'store_code_uc'))
    n_region <- CountUnique(dt[[m$regional_control]])
    out_list <- list()
    i <- 0
    for(cur_window in window_list){
      i <- i + 1
      cur_dt <- dt[YearQuarter %in% cur_window]
      cur_dt <- cur_dt[, count := CountUnique(get(paste0('price_', lhs_group))),
                       by = c(m$regional_control)
                       ][count >= 3]
      if(nrow(cur_dt) == 0) next
      out <- cur_dt[, EstimateRegion.TimeWindow(.SD, m),
                    by = c(m$regional_control)]
      out_list[[i]] <- out[, YearQuarter := cur_window[[1]]]

    }
    out <- rbindlist(out_list)
    out_summary <- ModelSpecificSummary(m, out, n_region, lhs_group = lhs_group)
    results[[lhs_group]] <- out_summary
  }
  setkeyv(move, old_key)
  return(results)
}

