#' Normalize values on a sphere
#'
#' @description Normalizes values of a vector such that the sum of squared elements equal to \eqn{r^2}.
#' @param x A vector or a matrix with values to be normalized.
#' @param r The diameter of a sphere, default 1.
#' @return It returns a data frame with normalized values.
#' @examples
#' normalizeRsphere(x = c(1, 0.5, 0.4))
#' @author Marjan Cugmas
#' @export

normalizeRsphere <- function (x,  r = 1) {
  if(is.vector(x)) r.sphere <- x * 1/sqrt(sum(x^2)) * r else r.sphere <- x * 1/sqrt(rowSums(x^2)) * r
  return(r.sphere)
}