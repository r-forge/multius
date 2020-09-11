correctDist <- function(matrika){
  razdalje <- matrix(nrow = dim(matrika)[1], ncol = dim(matrika)[2])
  for (i in 1:dim(matrika)[1]){
    for (j in 1:dim(matrika)[2]){
      razdalje[i, j] <-
        sqrt(
          ((matrika[i,i] - matrika[j,j])**2) +
            ((matrika[i,j] - matrika[j,i])**2) +
            sum(((matrika[,i] - matrika[,j])**2)[c(-i,-j)] +
                  ((matrika[i,] - matrika[j,])**2)[c(-i,-j)]))
    }
  }
  return(as.dist(razdalje))
}
