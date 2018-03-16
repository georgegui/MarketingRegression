#' @import Matrix
#' @import data.table
#' @import CommonFunctions
#' @import MatrixModels
NULL



#' @describeIn Estimate Demand Estimation Using Sparse OLS
#
#' @export Estimate.SparseOLS
Estimate.SparseOLS <- function(m, move){
  Estimate.Sparse(m, move, lm.Sparse.OLS)
}

#' @export
lm.Sparse.OLS <- function(x, y){
  out <- lm.FitSparseOLS(x, y)
  column_table <- attr(x, 'relevant_cols')
  out <- data.table(out[1:nrow(column_table)], column_table)
}

#' sparse regression, calculating the pvalue
#'
#' This function is a OLS of sparse matrix, it calculates the pvalue of the
#'  result, which was not available in the MatrixModels:::lm.fit.sparse function
#'
#' @param x a sparse matrix
#' @param y a vector
#' @param intercept whether to use intercept or not
#' @return a data.table with the coefficient and pvalue
#' @export
lm.FitSparseOLS <- function(x, y, intercept = FALSE){
  if(intercept){
    x <- cbind2(1, x)
  }
  coefficient = MatrixModels:::lm.fit.sparse(x,y)  # estimate coefficient
  prediction  = x%*%coefficient                    # estimated values
  e           = y - prediction                     # error term
  s2          = (t(e)%*%e)/(nrow(x) - ncol(x))     # variance of error term
  coef_var    = s2[1,1]*chol2inv(chol(t(x)%*%x))   # covariance matrix
  t           = coefficient/sqrt(diag(coef_var))   # calculate t value
  p           = 2*pt(-abs(t), nrow(x) - ncol(x))   # calculate pvalue
  return(data.table(coefficient = coefficient, p_value = p))
}


