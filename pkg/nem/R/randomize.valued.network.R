randomize.valued.network <- function(network, type){
  if (type == "weights.preserving"){
    network[lower.tri(network)] <- sample(network[lower.tri(network)])
    network[upper.tri(network)] <- t(network)[upper.tri(network)]
    return(network)
  }
  if (type == "degree.preserving" ){
    pogoj <- TRUE
    while (pogoj){
      O <- matrix(0, nrow = nrow(network), ncol = ncol(network))
      idealColSum <- colSums(network)
      initColSum <- rep(0, length(idealColSum))
      while (sum(idealColSum - initColSum == 0) != length(initColSum)){
        while(sum(idealColSum - initColSum == 0) < (length(idealColSum)-1)) {
          par <- sample(as.numeric(which(idealColSum > initColSum)), size = 2, replace = FALSE)
          O[par[1], par[2]] <- O[par[1], par[2]] + 1
          O[par[2], par[1]] <- O[par[2], par[1]] + 1
          initColSum[c(par[1], par[2])] <- initColSum[c(par[1], par[2])] - 1
          initColSum <- colSums(O)
        }
        if (sum(idealColSum - initColSum == 0) != length(initColSum)){
          par <- sample(as.numeric(which(idealColSum - initColSum ==0)), size = 2, replace = FALSE)
          O[par[1], par[2]] <- O[par[1], par[2]] - 1
          O[par[2], par[1]] <- O[par[2], par[1]] - 1
          initColSum[c(par[1], par[2])] <- initColSum[c(par[1], par[2])] + 1
          initColSum <- colSums(O)
        }
      }
      pogoj <- sum(O < 0) > 0
    }
    rownames(O) <- rownames(network)
    colnames(O) <- colnames(network)
    return(O)
  }
  if (type == "density.preserving"){
    # O <- matrix(0, nrow = nrow(network), ncol = ncol(network))
    # O[upper.tri(O)] <- round(prop.table(runif(nrow(network)*(nrow(network)-1)/2))*sum(network)/2)
    # O[lower.tri(O)] <- t(O[upper.tri(O)])
    # return(O)
    O <- matrix(0, nrow = nrow(network), ncol = ncol(network))
    for (i in 1:(sum(network)/2)){
      pair <- sample(1:nrow(O), size = 2, replace = FALSE)
      O[pair[1], pair[2]] <- O[pair[2], pair[1]] <- O[pair[1], pair[2]] + 1
    }
    rownames(O) <- rownames(network)
    colnames(O) <- colnames(network)
    return(O)
  }
}
