#' Calculate the value of the Relative Fit measure for generalized blockmodeling approach
#'
#' @description It calculates the value of the Relative Fit measure.
#' @param network The (binary) network in a matrix form.
#' @param nSim The number of randomized networks.
#' @param nCores The number of cores to be used by funcion \code{kmBlockORPC}.
#' @param groups The number of clusters in a blockmodel (single value of a vector of values).
#' @param bmType The list of image matrices.
#' @note The networks are randomized in such a way that loops are not allowed, but the density is
#' still the same as in the empirical networks (the degrees are not preserved)!
#' For asymmetric networks only!
#' @seealso \code{get.RF.kmeans}
#' @author Marjan Cugmas
#' @export

get.RF <- function(network,
                    nSim = 10,
                    nCores = 0,
                    groups = groups,
                    bmType = list("cohes" = cohes,"ccp" = ccp),
                    symmetrize = FALSE,
                    blockTypeWeights = 1,
                    rep = 200){
  #--- empiricni CF
  empCF <- matrix(NA, nrow = length(groups), ncol = length(bmType))
  rownames(empCF) <- groups
  if (is.null(names(bmType)) == FALSE) colnames(empCF) <- names(bmType)

  partitions <- array(data = NA, dim = c(nrow(network), length(groups), length(bmType)),
                      dimnames = list("units" = 1:nrow(network), "groups" = groups, "bmType" = names(bmType)))

  stevec <- 1
  for (i in groups){
    for (j in 1:length(bmType)){
      blocks <- bmType[[j]](i)

      resBM <- optRandomParC(M = network,
                             blockTypeWeights = blockTypeWeights,
                             k = i,
                             rep = rep,
                             approach = "bin",
                             blocks = blocks,
                             nCores = nCores)

      empCF[stevec, j] <- err(resBM)
      partitions[,stevec,j] <- clu(resBM)

    }
    stevec <- stevec+1
  }

  #--- rndRF
  rndRF <- array(data = NA, dim = c(length(groups), length(bmType), nSim))
  stevec <- 1
  for (i in groups){
    for (k in 1:nSim){
      randomized <- randomize(network)

      for (j in 1:length(bmType)){
        blocks <- bmType[[j]](i)
        rndRF[stevec, j, k] <- err(optRandomParC(M = randomized,
                                                 blockTypeWeights = blockTypeWeights,
                                                 k = i,
                                                 rep = rep,
                                                 approach = "bin",
                                                 blocks = blocks,
                                                 nCores = nCores))
      }
    }
    stevec <- stevec + 1
  }

  #--- RCF
  RF <- 1 - empCF / apply(rndRF, c(1, 2), mean)
  rownames(RF) <- groups
  if (is.null(names(bmType)) == FALSE) colnames(RF) <- names(bmType)

  return(list(network = network, partitions = partitions, empCF = empCF, rndCF = rndRF, RF = RF))
}
