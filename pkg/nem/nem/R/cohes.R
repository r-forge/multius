cohes <- function(nbl){
  B.cohes <- array(NA, dim = c(1, 1, nbl, nbl))
  B.cohes[1,1,,] <- matrix("null", nrow = nbl, ncol = nbl)
  diag(B.cohes[1,1,,]) <- "com"
  return(B.cohes)
}





