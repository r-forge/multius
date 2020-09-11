Hi_2 <- function(network, actor = NULL) {
  razdalje <- geodist(network, inf.replace = 0)$gdist
  omejene_razdalje <- (razdalje > 0) & (razdalje < 3)
  Hi <- colSums(omejene_razdalje)
  Hi <- prop.table(Hi)
  Hi[is.nan(Hi)] <- 0
  return(Hi)
}
