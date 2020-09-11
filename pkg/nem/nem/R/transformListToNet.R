transformListToNet <- function(net.list, n, from = 1, to = nrow(net.list)){
  omrezje <- matrix(0, nrow = n, ncol = n)
  for (i in from:to){
    omrezje[net.list[i,1], net.list[i,2]] <- net.list[i,3]  + omrezje[net.list[i,1], net.list[i,2]]
  }
  return(omrezje)
}
