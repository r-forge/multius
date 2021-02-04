#' Hierarchical blockmodel
#'
#' @description Hierarchical blockmodel.
#' @param nbl Number of clusters.
#' @author Marjan Cugmas
#' @export

hier <- function (nbl){
  nap <- cbind(c(2:nbl), c(1:(nbl - 1)))
  B.hier <- array(NA, dim = c(1, 1, nbl, nbl))
  B.hier[1, 1, , ] <- matrix("null", nrow = nbl, ncol = nbl)
  for (i in 1:nrow(nap)) B.hier[1, 1, , ][nap[i,1], nap[i,2]] <- "com"
  return(B.hier)
}
