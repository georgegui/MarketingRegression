#' @import data.table
#' @import CommonFunctions
NULL



#' Add specific parameters related to a model
#'
#' @export
#' @param model an estimation profile that specifies the parameters of the model
#' @return an updated estimation profile
AddModelSpecificParameters <- function(model, ...){
  UseMethod('AddModelSpecificParameters')
}
#' @describeIn AddModelSpecificParameters return the original model
#' @export
AddModelSpecificParameters.default <- function(model, ...){
  model
}

#' @describeIn AddModelSpecificParameters return the model with number of draws,
#'  thickness of MCMC runs, and whether run the estimation by certain region.
#' @export
AddModelSpecificParameters.Bayes <- function(model,
                                             n_draws           = 20000,
                                             thickness         = 10,
                                             estimate_by_group = NULL, 
                                             detail = F){
  valid_group_variables <- c('ZIP3', 'DMA', 'Chain', 'National')
  valid <- estimate_by_group %in% valid_group_variables
  if ((length(valid) == 0))
    valid = TRUE
  if (!valid)
    stop('invalid_estimate_by_group variable')
  if (IsTestMode()) {
    n_draws   = 2000
    thickness = 1
  }
  model$n_draws           <- n_draws
  model$thickness         <- thickness
  # model$estimate_by_group <- estimate_by_group
  return(model)
}

#' @describeIn AddModelSpecificParameters return the option related to sparse OLS
#' @export
AddModelSpecificParameters.SparseOLS <- function(model, ...){
  return(model)
}

#' @describeIn AddModelSpecificParameters return the option related to Unemployment
#' @export
AddModelSpecificParameters.TimeWindow <- function(model){
  # model$window <- period
  model$extra_cols <- 'YearQuarter'
  return(model)
}

