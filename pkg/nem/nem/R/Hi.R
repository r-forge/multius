Hi <- function(network, actor = NULL){
  pre<-colSums(geodist(network, inf.replace = 0)$gdist>0)
  Hi <- pre
  Hi <- prop.table(Hi)
  Hi[is.nan(Hi)] <- 0
  return(Hi)
}
