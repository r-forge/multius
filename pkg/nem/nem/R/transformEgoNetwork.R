transformEgoNetwork <- function (egocentric_network, symmetric = FALSE) {
  egocentric_network[is.na(egocentric_network)] <- 0
  egos <- unique(egocentric_network[, 1])
  n.egos <- length(egos)
  network <- matrix(0, nrow = n.egos, ncol = n.egos)
  rownames(network) <- colnames(network) <- egos
  for (i in 1:nrow(egocentric_network)) {
    actor <- which(is.element(colnames(network), egocentric_network[i, 1]) == TRUE)
    alters <- which(is.element(el = colnames(network), set = as.vector(as.character(egocentric_network[i, -1]))) == TRUE)
    network[actor, alters] <- 1 + network[actor, alters]
  }
  if (symmetric == TRUE) {
    network <- do.call("+", args = list(network, t(network)))
  }
  return(network)
}
