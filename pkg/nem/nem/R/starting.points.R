starting.points <- function(dim) {
  X1 <- matrix(0, nrow = dim, ncol = dim)
  diag(X1) <- 1

  X2 <- matrix(0, nrow = dim, ncol = dim)
  diag(X2) <- -1

  return(rbind(X1, X2))
}

