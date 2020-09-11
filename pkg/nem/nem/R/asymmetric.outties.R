asymmetric.outties <- function(network, actor){
  outcome <- which(network[actor,] == 1)[is.element(el = which(network[actor,] == 1), set = which(network[,actor] == 1)) == FALSE]
  egos <- rep(0, times = nrow(network))
  egos[outcome] <- 1
  return(egos)
}
