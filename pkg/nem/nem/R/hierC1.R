hierC1 <- function (nbl, transformForCritFunC = TRUE) {
  b <- matrix(NA, nrow = nbl, ncol = nbl)
  b[upper.tri(b)] <- "nul"
  diag(b) <- "com"
  b[lower.tri(b)] <- "nul"
  for (i in 2:nrow(b)) b[i, i-1] <- "com"
  b[nbl, nbl] <- "dnc"

  if (transformForCritFunC == TRUE) {
    r <- transform.for.critFunC(mat = b)
    return(r)
  }
  if (transformForCritFunC == FALSE) {
    return(b)
  }
}
