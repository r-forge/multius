transb <- function (nbl){
  B.transb <- array(NA, dim = c(1, 1, nbl, nbl))
  B.transb[1, 1, , ] <- matrix("null", nrow = nbl, ncol = nbl)
  B.transb[1, 1, , ][lower.tri(B.transb[1, 1, , ], diag = TRUE)] <- "com"
  return(B.transb)
}
