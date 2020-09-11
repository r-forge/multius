mutuality <- function(network, actor){
  asyminties <- as.numeric(network[, actor] + (network[actor, ]*-1) == 1)
  asyminties.relative <- asyminties/sum(asyminties)
  asyminties.relative[is.nan(asyminties.relative)] <- 0
  return(asyminties.relative)
}
