diff.popularity <- function(network, actor){
  popularity <- colSums(network)/nrow(network)
  relative.popularity <- popularity/sum(popularity)
  relative.popularity[is.nan(relative.popularity)] <- 0

  d.rel.pop <- (relative.popularity - relative.popularity[actor])**2
  d.rel.pop <- prop.table(d.rel.pop)
  d.rel.pop[is.nan(d.rel.pop)] <- 0

  return(d.rel.pop)
}


