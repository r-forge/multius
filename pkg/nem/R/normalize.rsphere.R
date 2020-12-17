#' Normalize values on a sphere
#'
#' @description Normalizes values of a vector such that the sum of squared elements equal to $r^2$.
#' @param x A vector or a matrix with values to be normalized.
#' @param r the diameter of a sphere, default 1.
#' @return It returns a data frame with normalized values.
#' @author Marjan Cugmas
#' @export

normalize.rsphere <- function (X,  r= 1) {
  if(is.vector(X)) r.sphere <- X * 1/sqrt(sum(X^2)) * r else r.sphere <- X * 1/sqrt(rowSums(X^2)) * r
  return(r.sphere)
}
