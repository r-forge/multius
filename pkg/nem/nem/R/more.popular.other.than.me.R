more.popular.others.than.me <- function(network, actor){
  all <- c(1:nrow(network))
  all <- rep(0, times = nrow(network))
  all[which(sum(network[, actor]) < colSums(network))] <- 1
  all[actor] <- 0
  return(all)
}
