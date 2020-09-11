correctDist <- function(data){
  razdalje <- matrix(nrow = dim(data)[1], ncol = dim(data)[2])
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      razdalje[i, j] <-
        sqrt(
          ((data[i,i] - data[j,j])**2) +
            ((data[i,j] - data[j,i])**2) +
            sum(((data[,i] - data[,j])**2)[c(-i,-j)] +
                  ((data[i,] - data[j,])**2)[c(-i,-j)]))
    }
  }
  return(as.dist(razdalje))
}
