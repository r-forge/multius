alterHi <- function(network, actor = NULL){
  pre<-colSums(geodist(network, inf.replace = 0)$gdist>0)
  if (is.null(actor) == TRUE) Hi <- pre
  if (is.null(actor) == FALSE) Hi <- (pre - pre[actor])**2
  Hi <- prop.table(Hi)
  Hi[is.nan(Hi)] <- 0
  return(Hi)
}
