popularity <- function(network, actor = NULL){
  popularity <- colSums(network)/nrow(network)
  relative.popularity <- popularity/sum(popularity)
  relative.popularity[is.nan(relative.popularity)] <- 0
  return(relative.popularity)
}

