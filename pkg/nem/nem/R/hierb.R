hierb <- function (nbl){
  nap <- cbind(c(2:nbl), c(1:(nbl - 1)))
  B.hierb <- array(NA, dim = c(1, 1, nbl, nbl))
  B.hierb[1, 1, , ] <- matrix("null", nrow = nbl, ncol = nbl)
  for (i in 1:nrow(nap)) B.hierb[1, 1, , ][nap[i,1], nap[i,2]] <- "com"
  diag(B.hierb[1, 1, , ]) <- "com"
  return(B.hierb)
}
