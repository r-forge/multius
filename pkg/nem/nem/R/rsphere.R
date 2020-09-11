rsphere <- function(n = 1000, dim = 3, r = 1, pos.only = FALSE){
  if (pos.only == FALSE){
    vred <- matrix(rnorm(dim*n), nrow = n, ncol = dim)
    r.sphere <- vred * 1/sqrt(rowSums(vred**2)) * r
    return(r.sphere)
  }

  if (pos.only == TRUE){
    vred <- abs(matrix(rnorm(dim*n), nrow = n, ncol = dim))
    r.sphere <- vred * 1/sqrt(rowSums(vred**2)) * r
    return(r.sphere)
  }
}

