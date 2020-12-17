#' Calculate the value of the Relative Fit measure for k-means blockmodeling approach
#'
#' @description The function calculate the value of the RF measure.
#' @param network The (binary) network in a matrix form.
#' @param nSim The number of randomized networks.
#' @param implFun The name of the function to be used for blockmodeling.
#' Currently implemented are options \code{"omkm"} and \code{"omkmNrep"} (from package \code{dBlockmodeling}) and
#' \code{"kmBlockORPC"} (from package \code{kmBlock}).
#' @param nCores The number of cores to be used by funcion \code{kmBlockORPC}.
#' @param \dots Additional parameters for function \code{omkm} or \code{kmBlockORPC}.
#' @author Marjan Cugmas
#' @export


get.RF.kmeans <- function(network, nSim, groups, implFun, nCores = 0, ...){
  partitions <- matrix(NA, nrow = nrow(network), ncol = length(groups))
  colnames(partitions) <- groups
  # EMPIRICNI RF
  empCF <- NULL
  for (i in 1:length(groups)){
    if (implFun == "omkm") {
      res <- omkm(A = network, RC = groups[i], ...)
      empCF[i] <- res$sse
      partitions[, i] <- res$RP
    }
    if (implFun == "omkmNrep") {
      res <- omkmNrep(A = network, RC = groups[i], ...)
      empCF[i] <- res$sse
      partitions[, i] <- res$RP
    }
    if (implFun == "kmBlockORPC") {
      res <- kmBlockORPC(M = network, k = groups[i], nCores = nCores, ...)
      empCF[i] <- err(res)
      partitions[, i] <- clu(res)
    }
  }
  names(empCF) <- groups

  # RF NA SLUCAJNIH OMREZJIH
  rndRF <- matrix(data = NA, nrow = nSim, ncol = length(groups))
  colnames(rndRF) <- groups
  for (i in 1:nrow(rndRF)) {
    randomized <- randomize(network)
    for (j in 1:ncol(rndRF)) {
      if (implFun == "omkm") rndRF[i, j] <- omkm(A = randomized, RC = groups[j], ...)$sse
      if (implFun == "omkmNrep") rndRF[i, j] <- omkmNrep(A = randomized, RC = groups[j], ...)$sse
      if (implFun == "kmBlockORPC") {
        res <- kmBlockORPC(M = randomized, k = groups[j], nCores = nCores, ...)
        rndRF[i, j] <- err(res)
      }
    }
  }

  RF <- 1 - empCF/colMeans(rndRF)
  names(RF) <- groups
  return(list(network = network, partitions = partitions, empCF = empCF, rndCF = rndRF, RF = RF))
}
