randomNetwork <- function(nUnits = 42, nTies = 546){
  network <- matrix(sample(rep(x = c(0, 1), times = c((nUnits*nUnits) - nTies, nTies)), size = nUnits*nUnits, replace = F), nrow = nUnits, ncol = nUnits)
  nloops <- sum(diag(network) == 1)
  diag(network) <- 1
  changeToOne <- sample(which(network == 0), size = nloops, replace = F)
  network[changeToOne] <- 1
  diag(network) <- 0
  return(network)
}
