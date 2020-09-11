asymmetric.inties <- function(network, actor){
  incom <- which(network[, actor] == 1)[is.element(el = which(network[, actor] == 1), set = which(network[actor, ] == 1)) == FALSE]
  egos <- rep(0, times = nrow(network))
  egos[incom] <- 1
  return(egos)
}
