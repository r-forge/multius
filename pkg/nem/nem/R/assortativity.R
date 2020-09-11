assortativity<-function (network, actor){
  diff <- abs(sum(network[, actor]) - colSums(network))^2
  max.deg <- apply(cbind(colSums(network), rep(sum(network[, actor]), nrow(network))), 1, max)
  all <- 1 - diff/4
  all<-ifelse(all<0,0,all)
  return(all)
}



