critFunCMC <- function(res, compIM){
  ERR <- critFunC(M = res$M,
                  approaches = res$initial.param$dots$approach,
                  clu = clu(res),
                  blocks = res$initial.param$dots$blocks,
                  blockTypeWeights = res$initial.param$dots$blockTypeWeights)

  im.ord <- minIncBlocks(res = res, compIM = compIM)$order
  return(sum(ERR$EM[,im.ord,im.ord] * ifelse(compIM=="dnc", yes = 0, no = 1)))
}
