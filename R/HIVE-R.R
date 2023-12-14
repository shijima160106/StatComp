#' @title A function computing $l_p/l_q$ norm of a matrix
#' @description A function computing $l_p/l_q$ norm of a matrix
#' @import LambertW
#' @import plyr
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @import MASS
#' @import matrixcalc
#' @import modelr
#' @import glmnet
#' @import expm
#' @import latex2exp
#' @import microbenchmark
#' @import knitr
#' @import kableExtra
#' @import boot
#' @import bootstrap
#' @param M the matrix to be calculated
#' @param p the parameter of the norm
#' @param q the parameter of the norm
#' @return the $l_p/l_q$ norm of a matrix
#' @examples
#' \dontrun{
#'     A <- matrix(11:16, nrow=3, ncol=2)
#'     lplqnorm(A, 1, 2)
#' }
#' @export
lplqnorm <- function(M, p, q) {
  store <- rep(0, nrow(M))
  for (i in 1:nrow(M)) {
    store[i] <- (lp_norm(M[i, ], q))^p
  }
  return (sum(store))^{1/p}
}

#' @title A Estimator of the number of factors using Eigenvalue Ratio method
#' @description A Estimator of the number of factors using Eigenvalue Ratio method
#' @param X the data matrix
#' @return Estimate of the number of factors
#' @examples
#' \dontrun{
#' X <- mvrnorm(n, rep(0, p), Sigma)
#' khat <- EigenvalueRatio(X)
#' }
#' @export
EigenvalueRatio <- function(X) {
  n <- nrow(X)
  m <- ncol(X)
  Sigmahat <- 1/n * t(X) %*% X
  values <- eigen(Sigmahat)$values
  kmax <- min(n, m)/2
  store <- rep(0, kmax)
  for (i in 1:kmax) {
    store[i] <- values[i]/values[i+1]
  }
  return(max(store))
}
