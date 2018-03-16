#' @import data.table
#' @import CommonFunctions
NULL


#' Select the LHS products of a model
#'
#' @export
SelectLhsProducts <- function(object, ...){
  UseMethod('SelectLhsProducts')
}

#' @describeIn SelectLhsProducts select subset of brands on LHS
#' @export
#' @param object a data.table of brands
#' @param total_revenue_covered the top brands that cover this percentage of
#' overall revenues will be selected.
#' @return  a data.table that includes brand selected
SelectLhsProducts.BrandBasket <- function(object,
  total_revenue_covered = .8,
  relevant_cols = c('group_code', 'module_code',
                   'size1_units', 'brand_code_uc_corrected')
  ){
  relevant_cols = intersect(names(object), relevant_cols)
  object <- object[revenue_cumshare < total_revenue_covered]
  object <- object[, .SD, .SDcols = relevant_cols]
}

#' select the RHS products of a model
#'
#' @export
SelectRhsProducts <- function(object, ...){
  UseMethod('SelectRhsProducts')
}
#' @describeIn SelectRhsProducts select subset of brands on RHS
#' @export
#' @inheritParams SelectRhsProducts.BrandBasket
#' @param relevant_cols the columns to be kept
#' @param module_revenue_covered the top brands that cover this percentage of
#' category revenues will be selected.
#' @return  a data.table that includes brand selected
SelectRhsProducts.BrandBasket <- function(brand_dt,
  module_revenue_covered = .8){
  relevant_cols <- c('group_code', 'module_code',
    'brand_revenue_rank', 'brand_revenue',
    'brand_code_uc_corrected', 'size1_units')
  brand_dt[, cum_sum_module :=
    cumsum(brand_revenue)/sum(brand_revenue, na.rm = TRUE),
    by = .(module_code)
    ]
  brand_dt[, exceed_revenue_cap :=
    cumsum(cum_sum_module > module_revenue_covered),
    by = .(module_code)
    ]
  brand_dt <- brand_dt[exceed_revenue_cap <= 1]
  relevant_cols <- intersect(relevant_cols, names(brand_dt))
  brand_dt <- brand_dt[, .SD, .SDcols = relevant_cols]
  brand_dt[, rank_within_module := 1:.N, by = module_code]
}

#' select the list of modules to be estimated
#' @export
SelectCategories <- function(object, ...){
  UseMethod('SelectCategories')
}
#' @describeIn SelectCategories select modules in brand estimation
#' @export
SelectCategories.BrandBasket <- function(brand_dt, movement_path,
                                         module_file_type = 'file'){
  selected_modules     <- unique(brand_dt$module_code)
  if(module_file_type == 'file'){
    existing_modules     <- CheckModuleFileExistence(movement_path, '*.RData')
    existing_modules     <- existing_modules$module_number
  } else if(module_file_type == 'directory'){
    existing_modules <- dir(movement_path)
  } else {
    stop('module file should be either file or directory')
  }
  intersect_modules    <- intersect(selected_modules, existing_modules)
  non_existing_modules <- setdiff(selected_modules, intersect_modules)
  i <- length(non_existing_modules)
  if(i > 0) warning(Separator(), i, ' module files do not exist\n')
  VerboseWarning(paste(non_existing_modules, collapse = ', '),
                 'does not exist in file')
  return(intersect_modules)
}

#' select a subset of price movement data
#' @export
SelectMovement <- function(object, ...){
  UseMethod('SelectMovement')
}
#' @describeIn SelectMovement select subset of price movememnt
#'  in demand estimation
#' @export
SelectMovement.BrandPrice <- function(move, m){
  groups_to_keep <- union(m$lhs_groups, m$rhs_groups)
  move <- move[group_code %in% groups_to_keep]
  move <- move[year(week_end) %between% c(m$year_start, m$year_end)]
  if(IsTestMode()){
    most_observed_dma <- move[, .N, by = DMA][order(-N)][1, DMA]
    move <- move[DMA == most_observed_dma]
  }
  Message <- function(){
    paste('Processing', CountUnique(move[, store_code_uc]), 'stores and',
      CountUnique(move[, group_code]), 'brands after filtering')
  }
  old_n_store_number <- CountUnique(
    move[, .GRP, by = .(store_code_uc, group_code)]$GRP)
  # file store brand week
  move <- move[!is.na(get(m$base_price_col))]
  VerboseWarning(Message(), 'store brand week with NA base price')
  move <- move[!is.na(get(m$price_col))]
  VerboseWarning(Message(), 'store brand week with NA imputed price')
  if(m$remove_0_sale_weeks){
    mode <- move[get(m$sales_col) > 0]
    VerboseWarning(Message(), 'store brand week with no sales')
  }
  # filter store brand
  move <- move[, count := .N,
               by = .(store_code_uc, group_code)
               ][count >= m$min_nweeks_per_store_brand
                 ][, count := NULL]
  VerboseWarning(Message(), 'store brands with few weeks')
  move <- move[, count := CountUnique(get(m$price_col)),
               by = .(store_code_uc, group_code)
               ][count > m$min_nprices_per_store_brand
                 ][, count := NULL]
  VerboseWarning(Message(), 'store brands with little price fluctuations')
  move <- move[, count :=
                 sum(get(m$sales_col) == 0, na.rm = TRUE) / .N,
               by = .(store_code_uc, group_code)
               ][count >= m$min_nonzero_sales_perc_per_store_brand
                 ][, count := NULL]
  VerboseWarning(Message(), 'store brands with too many 0 sales')
  # filter brand

  move <- move[, count := CountUnique(store_code_uc),
    by = .(group_code)
    ][count >= m$min_nstores_per_brand
    ][, count := NULL]
  VerboseWarning(Message(), 'brands with few stores')
  # filter Chain
  move <- move[, count := CountUnique(store_code_uc),
    by = .(Chain)
    ][count >= m$min_nstores_per_chain
    ][, count := NULL]
  VerboseWarning(Message(), 'chains with few stores')
  move <- move[, count := CountUnique(ZIP3),
               by = .(Chain)
               ][count >= m$min_nzip3s_per_chain
                 ][, count := NULL]
  VerboseWarning(Message(), 'Chains with few ZIP3')
  # filter ZIP3 brand
  move <- move[, count := CountUnique(store_code_uc),
    by = .(ZIP3, group_code)
    ][count >= m$min_nstores_per_zip3
    ][, count := NULL]
  VerboseWarning(Message(), 'ZIP3 with few stores')

  cat(CountUnique(move[, store_code_uc]), 'stores remaining after filtering\n')
  new_n_store_number <- CountUnique(
    move[, .GRP, by = .(store_code_uc, group_code)]$GRP)
  attr(move, 'percentage_filtered') <- 1 - new_n_store_number/old_n_store_number
  return(move)
}

#' Select the Competing Brands based on model specification and own_group
#'
#' @export
SelectCompetingProducts <- function(own_group, m){
  if(own_group %in% m$rhs_groups){
    competing_groups <- m$rhs_groups[1:min(5, length(m$rhs_groups))]
  } else {
    competing_groups <- m$rhs_groups[1:min(4, length(m$rhs_groups))]
  }
  union(competing_groups, own_group)
}

#' select the list of right hand side variables such as promotion and price
#' variables based on a model
#' @export
SelectRhsVariables <- function(m){
  if(m$promotion_own | m$promotion_all){
    promotion_col <- 'promotion'
  } else {
    promotion_col <- NULL
  }
  if(m$promotion_interaction_own | m$promotion_interaction_all){
    promotion_interaction_col <- 'promotion_interaction'
  } else {
    promotion_interaction_col <- NULL
  }
  c('price', promotion_col, promotion_interaction_col)
}


#' Select potential competing brands of a category
#'
#' This function selects the potential competing brands of a category.
#'  based on the
#'  n_max_competing_brands value of the estimation profile.
#'
#' @param brand_dt a data.table of class BrandBasket, which
#'  includes the revenue rank of a brand
#' @param model the estimation profile
#' @return a list of selected brands
#' @export
SelectCategoryRhsProducts <- function(brand_dt, model){
  if(!('BrandBasket' %in% class(brand_dt))){
    stop('invalid brand table input')
  }
  old_keys      <- key(brand_dt)
  correct_keys  <- c("module_code", "brand_revenue_rank")
  correct_order <- identical(old_keys, correct_keys)
  if(!correct_order){
    setkey(brand_dt, correct_keys)
  }
  n_max_brands <- model$n_max_competing_brands
  if(nrow(brand_dt) > n_max_brands){
    selected_brands <- brand_dt[1:n_max_brands, group_code]
  } else {
    selected_brands <- brand_dt[, group_code]
  }
  if(!correct_order){
    setkey(brand_dt, old_keys)
  }
  return(selected_brands)
}



#' Select All Valid Control Variables
#'
#' @export
#'
#' @param m estimation profile that includes the type of 'promotion',
#'  'interaction' that are used.
#' @param lhs_group the lhs_group of the current estimation.
#' @param rhs_groups a list of potential competing groups that are valid and
#'  also exist in the current data.
SelectControls <- function(m, lhs_group, rhs_groups){
  promotion <- NULL
  if(m$promotion_own) promotion <- paste('promotion', lhs_group, sep = '_')
  if(m$promotion_all) promotion <- paste('promotion', rhs_groups, sep = '_')
  p_interaction <- NULL
  if(m$promotion_interaction_own){
    p_interaction <- paste('promotion_interaction', lhs_group, sep = '_')
  }
  if(m$promotion_interaction_all){
    p_interaction <- paste('promotion_interaction', rhs_groups, sep = '')
  }
  price <- paste('price', rhs_groups, sep = '_')
  c(price, promotion, p_interaction)
}
