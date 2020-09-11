more.popular.others <- function(network, actor){
  friends <- intersect(which(network[, actor] == 1), which(network[actor, ] == 1))
  if (length(friends) == 0) others.pop <- mean(sum(network[, c(actor)])) - colSums(network[, c(1:nrow(network))])
  if (length(friends) == 1) others.pop <- mean(sum(network[, c(actor, friends)])) - colSums(network[, c(1:nrow(network))])
  if (length(friends) >= 2) others.pop <- mean(colSums(network[, c(actor, friends)])) - colSums(network[, c(1:nrow(network))])
  others.pop[friends] <- 0
  others.pop[others.pop <= 0] <- 0
  others.pop[others.pop > 0] <- 1
  return(others.pop)
}
