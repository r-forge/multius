numberOfClusters <- function(x, w = 0.4){
  razl <- diff(x)
  ff <- NULL
  for (i in 1:(length(razl)-1)){ff[i] <- razl[i+1] <= razl[i]*w}
  return(which.max(which(ff==TRUE)))
}
