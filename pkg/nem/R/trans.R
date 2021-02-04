#' Transitivity blockmodel
#'
#' @description Transitivity blockmodel.
#' @param nbl Number of clusters.
#' @author Marjan Cugmas
#' @export

trans <- function (nbl){
  B.trans <- array(NA, dim = c(1, 1, nbl, nbl))
  B.trans[1, 1, , ] <- matrix("null", nrow = nbl, ncol = nbl)
  B.trans[1, 1, , ][lower.tri(B.trans[1, 1, , ])] <- "com"
  return(B.trans)
}
