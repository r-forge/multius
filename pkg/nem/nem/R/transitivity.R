transitivity <- function(network, actor){
  network <- network + t(network)
  network[network > 0] <- 1
  counts <- NULL
  for (i in 1:nrow(network)){
    friends <-  cbind(network[i, ],
                      network[, i],
                      network[actor, ],
                      network[, actor])
    counts[i] <- sum(rowSums(friends) == 4)
  }
  if (sum(counts) != 0) counts <- counts/sum(counts)
  return(counts)
}


