gen.network <- function(type, errors, n = 40, symmetric = TRUE, loop = FALSE, size = "equal"){
  new.network <- matrix(NA, nrow = n, ncol = n)
  if (is.vector(size)==FALSE){
    n.for.each.block <- n/nrow(type)
    spodnje <- seq(from = 1, to = n, by = n.for.each.block)
    zgornje <- seq(from = 0, to = n, by = n.for.each.block)[-1]
  }
  if (is.vector(size)==TRUE){
    if (sum(size) != 1) warning("The size should sum to 1.")
    if (nrow(type) != length(size)) warning("The size of each block has to be specified.")
    n.for.each.block <- n*size
    meje <- matrix(NA, nrow = 2, ncol = nrow(type))
    meje[1, 1] <- 1
    meje[2,  ] <- cumsum(n.for.each.block)
    meje[1, 2:ncol(meje)] <- cumsum(n.for.each.block)[1:(ncol(meje)-1)]+1
    spodnje <- meje[1,]
    zgornje <- meje[2,]
  }
  if (is.matrix(errors) == FALSE) errors <- matrix(errors, nrow = dim(type)[1], ncol = dim(type)[2])
  for (i in 1:nrow(type)){
    for (j in 1:ncol(type)){
      if (type[i, j] == "com"){
        n.of.cells <- length(spodnje[i]:zgornje[i])*length(spodnje[j]:zgornje[j])
        new.network[spodnje[i]:zgornje[i], spodnje[j]:zgornje[j]] <- matrix(sample(x = c(0, 1), size = n.of.cells, replace = T, prob = c(errors[i,j], 1 - errors[i,j])), nrow = length(spodnje[i]:zgornje[i]), ncol = length(spodnje[j]:zgornje[j]))
      }
      if (type[i, j] == "null"){
        n.of.cells <- length(spodnje[i]:zgornje[i])*length(spodnje[j]:zgornje[j])
        new.network[spodnje[i]:zgornje[i], spodnje[j]:zgornje[j]] <- matrix(sample(x = c(0, 1), size = n.of.cells, replace = T, prob = c(1 - errors[i,j], errors[i,j])), nrow = length(spodnje[i]:zgornje[i]), ncol = length(spodnje[j]:zgornje[j]))
      }
    }
  }
  if (symmetric == TRUE) new.network[lower.tri(new.network)] = t(new.network)[lower.tri(new.network)]
  if (loop == FALSE) diag(new.network) <- 0
  return(new.network)
}
