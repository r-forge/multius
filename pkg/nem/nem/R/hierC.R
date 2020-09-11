hierC <- function(nbl, transformForCritFunC = TRUE) {
  b <- matrix(NA, nrow = nbl, ncol = nbl)
  diag(b) <- "com"
  b[nbl, nbl] <- "dnc"
  b[lower.tri(b)] <- "dnc"
  for (i in 1:nrow(b)-1) b[i+1, i] <- "com"
  b[upper.tri(b)] <- "nul"
  if (transformForCritFunC == TRUE) {
    r <- transform.for.critFunC(mat = b)
    return(r)
  }
  if (transformForCritFunC == FALSE) {
    return(b)
  }
}

