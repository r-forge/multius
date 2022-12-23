#' Network density based on an image matrix and a partition
#'
#' @description Based on an image matrix and a partition it calculate the density of a whole network. 
#' @param M Image matrix with block densities.
#' @param partition A partition in a vector format. Each unique value (positive integers) represents one cluster.
#' @return Density of a whole network (a single value).
#' @examples
#' M <- matrix(c(0.1, 0.4, 0.5, 0.3), nrow = 2)
#' partition <- c(1, 2, 2, 1, 1, 2, 2, 2, 1)
#' globalDensity(M = M, partition = partition)
#' @author Marjan Cugmas
#' @export

globalDensity <- function(M, partition) {
posamezne <- NULL
  for (i in 1:nrow(M)) {
    for (j in 1:ncol(M)) {
      posamezne <- c(posamezne, c(M[i,j] * sum(partition == i)**2))
    }
  }
  return(sum(posamezne)/length(partition)**2)
}
