randomize <- function(network){
  random <- matrix(sample(network), nrow = nrow(network))
  ndiag <- sum(diag(random))
  diag(random) <- -1

  random[sample(which(random == 0), replace = FALSE, size = ndiag)] <- 1
  diag(random) <- 0
  return(random)
}
