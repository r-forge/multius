binarize <- function(network, type = "simple", symmetrize = TRUE){
  egoNames <- rownames(network)
  if (type == "simple") {
    network <- matrix(as.numeric(network > 0), nrow = nrow(network))
  }
  if (type == "Schaefer") {
    network <- matrix(as.numeric(network/rowSums(network) > 1/(nrow(network) - 1)), nrow = nrow(network))
    network[is.na(network)] <- 0
  }
  if (symmetrize){
    network <- do.call("+", args = list(network, t(network)))
    network <- matrix(as.numeric(network > 0), nrow = nrow(network))
  }
  colnames(network) <- rownames(network) <- egoNames
  return(network)
}
