less.popular.others.than.me <- function(network, actor){
  all <- rep(0, times = nrow(network))
  all[which(sum(network[, actor]) > (colSums(network))+2)] <- 1

  all[actor] <- 0
  return(all)
}
