common.friends <- function(network, actor, as.symmetric = TRUE){
  if (as.symmetric == TRUE) {
    network <- network + t(network)
    network[network > 0] <- 1
  }
  counts <- NULL
  for (i in 1:nrow(network)){
    friends <-  cbind(network[i, ],
                      network[, i],
                      network[actor, ],
                      network[, actor])
    counts[i] <- sum(rowSums(friends) == 4)
  }
  return(counts)
}
