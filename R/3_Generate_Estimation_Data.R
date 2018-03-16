#' @import data.table
#' @import CommonFunctions
NULL

#' Generate Estimation Data for Test
#'
#' @export
GenerateEstimationTestData <- function(
  n_store = 30,
  n_brands = 7,
  n_zip3 = 1,
  region_time_fe = TRUE,
  own_elas_draw = function() -2,
  promotion_draw = function() 1,
  competing_elas_draw = function() 0.2,
  missing_assortment_draw = function() rbinom(1, 1, 0.1)
){
  set.seed(6)
  week_time_table <- GenerateTimeFactorTable()
  dt <- expand.grid(
    week_end = week_time_table$week_end,
    ZIP3 = as.character(1:n_zip3),
    store_code_uc = as.character(1:n_store),
    group_code = as.character(1:n_brands)
  )
  dt <- data.table(merge(dt, week_time_table[, .(week_end, YearMonth)]))
  dt[, price         := rnorm(.N)]
  dt[, group_delete  := missing_assortment_draw(), by = .(store_code_uc, group_code)]
  dt <- dt[group_delete == 0]
  # dt[, price := price + rnorm(1), by = .(group_code, YearMonth)]
  dt[, store_fixed_effect := rnorm(1), by = .(group_code, store_code_uc)]
  dt[, promotion_cutoff   := quantile(price, 0.60),
     by = .(group_code, store_code_uc)]
  dt[, promotion              := as.numeric(price > promotion_cutoff)]
  dt[, promotion_effect       := promotion_draw(), by = .(group_code, store_code_uc)]
  dt[, competing_elasticity   := competing_elas_draw(), by = .(group_code, store_code_uc)]
  dt[, own_elasticity         := own_elas_draw(), by = .(group_code, store_code_uc)]
  dt[, competing_price_effect := sum(competing_elasticity * price),
     by = .(store_code_uc, week_end, ZIP3)]

  sales_vector <- rowSums(dt[, .(own_elasticity * price,
                                 competing_price_effect,
                                 -competing_elasticity * price,
                                 promotion_effect * promotion)],
                          na.rm = TRUE)

  dt[, sales := sales_vector]
  dt <- dt[sales != 0]
  dt[, sales := sales + store_fixed_effect, by = store_code_uc]
  if(region_time_fe){
    dt[, sales := sales  + rnorm(1),
       by = .(group_code, YearMonth, ZIP3)]
  }
    return(dt)
}


#' Generate data ready for estimation
#'
#' @export
FormatDataForEstimation <- function(object, ...){
  UseMethod('FormatDataForEstimation')
}

#' @title Generate estimation data ready for Hierarchical bayes
#' @name FormatDataForEstimation
## @method FormatDataForEstimation Demand Estimation for Brand Price
#' @export FormatDataForEstimation.BrandPrice
FormatDataForEstimation.BrandPrice <- function(move, m){
  move[, promotion := get(m$price_col) / get(m$base_price_col) < 0.95]
  move[, promotion := as.numeric(promotion)]
  if(m$log_price){
    move[, price := log(get(m$price_col))]
  } else {
    move[, price := get(m$price_col)]
  }
  if(m$log_sales){
    move[, sales := log(get(m$sales_col) + m$log_sales_added_by_1)]
  } else {
    move[, sales = get(m$sales_col)]
  }
  if(m$promotion_interaction_own | m$promotion_interaction_all){
    move[, promotion_interaction := promotion * price]
    interaction_col <- 'promotion_interaction'
  } else {
    interaction_col <- NULL
  }
  relevant_cols <- c('price', 'promotion', 'sales', interaction_col)
  by_cols <- c(m$time_control, m$regional_control, 'group_code')
  if(m$estimate_by_group != 'National') by_cols <- c(by_cols, m$estimate_by_group)
  if(m$demean_before_estimation){
    move <- DemeanCols(move, relevant_cols, by_cols)
    move <- DemeanCols(move, relevant_cols, c('store_code_uc', 'group_code'))
  }
  cols_to_return <- unique(c(relevant_cols, by_cols, 'store_code_uc', 'week_end', m$extra_cols))
  move           <- move[, .SD, .SDcols = cols_to_return]
  class(move)    <- c(m$estimation_type, class(move))
  return(move)
}

