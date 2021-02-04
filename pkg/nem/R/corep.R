#' Symmetric core-periphery blockmodel
#'
#' @description Symmetric core-periphery blockmodel.
#' @param nbl Number of clusters.
#' @author Marjan Cugmas
#' @export

corep <- function (nbl){
  B.corep <- array(NA, dim = c(1, 1, nbl, nbl))
  B.corep[1, 1, , ] <- matrix("null", nrow = nbl, ncol = nbl)
  B.corep[1, 1, , ][, 1] <- "com"
  B.corep[1, 1, , ][1, ] <- "com"
  return(B.corep)
}
