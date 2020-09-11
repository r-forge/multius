diff.Hi <- function(network, actor){
  pre<-colSums(geodist(network, inf.replace = 0)$gdist>0)
  Hi <- (pre - pre[actor])**2
  Hi <- prop.table(Hi)
  Hi[is.nan(Hi)] <- 0
  return(Hi)
}
