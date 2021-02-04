#' Asymmetric core-cohesive blockmodel
#'
#' @description Asymmetric core-cohesive blockmodel.
#' @param nbl Number of clusters.
#' @author Marjan Cugmas
#' @export

corepas <- function (nbl){
  B.corepas <- array(NA, dim = c(1, 1, nbl, nbl))
  B.corepas[1, 1, , ] <- matrix("null", nrow = nbl, ncol = nbl)
  B.corepas[1, 1, , ][, 1] <- "com"
  return(B.corepas)
}
