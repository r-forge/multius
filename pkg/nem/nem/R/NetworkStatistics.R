NetworkStatistics <- function(net, actor, internal.mechanisms, external.mechanisms, external.covariates, theta.im, theta.em){
  X <- matrix(NA, nrow = nrow(net), ncol = length(internal.mechanisms))
  for (j in 1:length(internal.mechanisms)) {
    X[, j] <- internal.mechanisms[[j]](net, actor = actor)
  }
  Y <- matrix(NA, nrow = nrow(net), ncol = length(external.mechanisms))
  for (j in 1:length(external.mechanisms)) {
    Y[, j] <- external.mechanisms[[j]](covariate = external.covariates[[j]], actor = actor)
  }
  XY <- cbind(X, Y)
  XY[is.nan(XY)] <- 0
  probabilities <- XY %*% c(theta.im, theta.em)
  return(probabilities)
}
