asymtransitivity <- function(network, actor){
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
