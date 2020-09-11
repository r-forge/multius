diff.Hi_3 <- function(network, actor){
  razdalje <- geodist(network, inf.replace = 0)$gdist
  omejene_razdalje <- (razdalje > 0) & (razdalje < 4)
  Hi <- colSums(omejene_razdalje)

  Hi <- (Hi - Hi[actor])**2
  Hi <- prop.table(Hi)
  Hi[is.nan(Hi)] <- 0
  return(Hi)
}

