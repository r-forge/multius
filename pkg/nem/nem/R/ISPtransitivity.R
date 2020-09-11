ISPtransitivity <- IncomingSharedPartner <- function (network, actor) {
  counts <- NULL
  for (i in 1:nrow(network)) {
    friends <- cbind(network[, actor], network[, i])
    counts[i] <- sum(rowSums(friends) == 2)
  }
  counts[actor] <- 0
  if (sum(counts) != 0) counts <- counts/sum(counts)
  return(counts)
}
