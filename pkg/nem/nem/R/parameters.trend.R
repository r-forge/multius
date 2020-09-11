parameters.trend <- function(x, div = 5, old = TRUE){
  theta.mat <- matrix(0, nrow = 10, ncol = 4)
  theta.mat[1,] <- x
  change <- abs(theta.mat[1,1]/div)

  theta.mat[2:10, 1] <- x[1] - cumsum(rep(change, 9))
  theta.mat[2:10, 2] <- x[2] + cumsum(rep(change, 9))
  theta.mat[, 3] <- x[3]
  theta.mat[, 4] <- x[4]
  if (old == FALSE) for (i in 1:10) theta.mat[i, ] <- to.sphere(theta.mat[i,])
  return(theta.mat)
}
