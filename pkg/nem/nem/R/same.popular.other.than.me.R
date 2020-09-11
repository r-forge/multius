same.popular.others.than.me <- function(network, actor, error = 0){
  all <- rep(0, times = nrow(network))

  interval <- c(sum(network[, actor]) - error, sum(network[, actor]) + error)

  all[which(colSums(network) >= interval[1] & colSums(network) <= interval[2])] <- 1
  all[actor] <- 0
  return(all)
}
