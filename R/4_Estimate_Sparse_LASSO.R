#' @import Matrix
#' @import data.table
#' @import CommonFunctions
#' @import glmnet
NULL

#' @describeIn Estimate Demand Estimation Using Sparse LASSO
#
#' @export Estimate.SparseLASSO
Estimate.SparseLASSO <- function(m, move){
  Estimate.Sparse(m, move, lm.Sparse.LASSO)
}

#' @export
lm.Sparse.LASSO <- function(x, y, intercept = FALSE){
  #
  column_table <- attr(x, 'relevant_cols')
  price_penalties <- rep(1, nrow(column_table))
  time_penalties <- rep(1, attr(x, 'n_month_cols'))
  store_penalties <- rep(0, attr(x, 'n_store_cols') - 1)
  penalties <- c(price_penalties, time_penalties, store_penalties, 1)
  out <- cv.glmnet(x, y, nfolds = 3,
                   intercept = TRUE, exclude = ncol(x),
                   penalty.factor = penalties,
                   standardize = TRUE)
  best_coef = as.numeric(coef(out, s = 'lambda.min'))[-1]
  coef_table = data.table(coefficient = best_coef,
                significant = best_coef != 0)
  data.table(coef_table[1:nrow(column_table)], column_table)
}

