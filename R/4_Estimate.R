#' @import data.table
NULL

#' Estimate result based on a model specification and data
#'
#' @export
#' @param model an estimation profile
#' @param data
#' a data.table with necessary columns for estimation
#' \itemize{
#'   \item 'group_code': the product, brand, or category id, including the competing products involved.
#'   \item 'sales': the unit sales, or the RHS variable of the estimation
#'   \item 'price': the price of the products
#'   \item time control: specified as time_control in \code{model}, such as column with names 'YearMonth', 'YearQuarter'
#'   \item region control: specified as regional_control in \code{model}, such as column with names 'ZIP3', 'DMA'
#' }
#' sales:
#'   time control column:
#'   region control column: specified as time_control in \code{model}, such as column of names 'YearMonth', 'YearQuarter'
#'
#'
Estimate <- function(model, data, ...){
  UseMethod('Estimate')
}

#' Produce summary related to a model
#'
#' Choose a list of objects to store for the estimation results.
#' It is a helper function for Estimate.
#'
#' @param model estimation profile
#' @param out the unformatted estimation result
#' @return formatted estimation result
#' @export
ModelSpecificSummary <- function(model, out, ...){
  UseMethod('ModelSpecificSummary')
}

#' @describeIn ModelSpecificSummary default
#' @export
ModelSpecificSummary.default <- function(model, out, n_store,...){
  if(is.null(out)){
    success_rate = 0
  } else {
    success_rate = CountUnique(out[, store_code_uc])/n_store
  }
  result <- list(
    success   = success_rate,
    test_mode = IsTestMode(),
    model     = model,
    out       = out,
    ...
  )
  class(result) <- paste0(class(model)[[1]], '.Result')
  return(result)
}

#' @describeIn ModelSpecificSummary Unemployment
#' @export
ModelSpecificSummary.TimeWindow <- function(model, out, n_region,...){
  if(is.null(out)){
    success_rate = 0
  } else {
    success_rate = CountUnique(out[[model$regional_control]])/n_region
  }
  result <- list(
    success   = success_rate,
    test_mode = IsTestMode(),
    model     = model,
    out       = out,
    ...
  )
  class(result) <- paste0(class(model)[[1]], '.Result')
  return(result)
}

