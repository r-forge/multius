get.RCF <- function(network,
                    nSim = 10,
                    nCores = 0,
                    blocks = blocks,
                    groups = groups,
                    bmType = list("cohes" = cohes,"ccp" = ccp),
                    symmetrize = FALSE, ...){
  #--- empiricni CF
  sym.network <- network + t(network); sym.network[sym.network > 1] <- 1

  empCF <- matrix(NA, nrow = length(groups), ncol = length(bmType))
  stevec <- 1
  for (i in groups){
    for (j in 1:length(bmType)){
      blocks <- bmType[[j]](i)
      empCF[stevec, j] <- err(optRandomParC(M = ifelse(test = symmetrize == TRUE, yes = list(sym.network), no = list(network))[[1]],
                                            k = i,
                                            rep = 100,
                                            approach = "bin",
                                            blocks = blocks,
                                            nCores = nCores))
    }
    stevec <- stevec+1
  }

  #--- rndCF
  rndCF <- array(data = NA, dim = c(length(groups), length(bmType), nSim))
  stevec <- 1
  for (i in groups){
    for (k in 1:nSim){
      randomized <- randomize(network)
      sym.randomized <- randomized + t(randomized); sym.randomized[sym.randomized > 1] <- 1

      for (j in 1:length(bmType)){
        blocks <- bmType[[j]](i)
        rndCF[stevec, j, k] <- err(optRandomParC(M = ifelse(test = symmetrize == TRUE, yes = list(sym.randomized), no = list(randomized))[[1]],
                                                 k = i,
                                                 rep = 100,
                                                 approach = "bin",
                                                 blocks = blocks,
                                                 nCores = nCores))
      }
    }
    stevec <- stevec + 1
  }

  #--- RCF
  RCF <- 1 - empCF / apply(rndCF, c(1, 2), mean)
  rownames(RCF) <- groups
  if (is.null(names(bmType)) == FALSE) colnames(RCF) <- names(bmType)
  return(list(empCF=empCF, rndCF=rndCF, RCF=RCF))
}
