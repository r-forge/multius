Di <- function(network, actor){
  suppressWarnings(d <- unlist(lapply(shortest_paths(graph = graph_from_adjacency_matrix(as.matrix(network)), from = actor)$vpath, length)))
  d[d == 0] <- max(d) + 1
  d <- d - 1
  d<-prop.table(d)
  d[is.nan(d)] <- 0
  return(d)
}
