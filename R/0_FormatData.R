
#' SetTableFormat Format the brand table
#'
#' @export
#'
SetBrandTableFormat <- function(DT, data_type = 'old'){
  if(data_type == 'old'){
    DT <- SetTableFormat(DT,
                         old_names = c('brand_group_code',
                                       'product_module_code',
                                       'grouped_brand_RMS_revenue_cumshare',
                                       'grouped_brand_RMS_revenue_rank',
                                       'RMS_revenue_brand_grouped',
                                       'brand_group_descr'),
                         new_names = c('group_code',
                                       'module_code',
                                       'revenue_cumshare',
                                       'brand_revenue_rank',
                                       'brand_revenue',
                                       'brand_descr'),
                         cols_to_keep = c('product_module_descr'),
                         cols_to_factor = c('product_module_code', 'brand_group_code')
    )
  }
  if(data_type == 'new'){
    DT <- SetTableFormat(DT,
                         old_names = c('product_module_code',
                                       'revenue_RMS',
                                       'brand_descr_corrected'),
                         new_names = c('module_code',
                                       'brand_revenue',
                                       'brand_descr'),
                         cols_to_keep = c('size1_units', 'product_module_descr',
                                          "revenue_cumshare", "brand_revenue_rank",
                                          'brand_code_uc_corrected',
                                          'department_descr'),
                         cols_to_factor = c("product_module_code"))
    DT[, group_code := as.character(.GRP),
       by = .(brand_code_uc_corrected, size1_units, module_code)]
  }
  setkey(DT, module_code, brand_revenue_rank)
  setattr(DT, 'class', c('BrandBasket', class(DT)))
  return(DT)
}




#' SetTableFormat Format price movement table
#'
#' @export
#'
SetBrandMovementFormat <- function(DT){
  DT <- SetTableFormat(DT,
                       old_names      = 'brand_group_code',
                       new_names      = 'group_code',
                       cols_to_keep   = names(DT),
                       cols_to_factor = c('brand_group_code', 'store_code_uc')
  )
  setattr(DT, 'class', c('BrandPrice', class(DT)))
}
