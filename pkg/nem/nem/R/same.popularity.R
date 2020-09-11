same.popularity <- function(network, actor){
  all <- rep(0, times = nrow(network))

  interval <- c(sum(network[, actor]), sum(network[, actor]))

  all[which(colSums(network) >= interval[1] & colSums(network) <= interval[2])] <- 1
  all[actor] <- 0
  all <- all/sum(all)
  all[is.nan(all)] <- 0
  return(all)
}


