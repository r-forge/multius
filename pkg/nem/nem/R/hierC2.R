hierC2 <- function(nbl, transformForCritFunC = TRUE){
  b <- matrix(NA, nrow = nbl, ncol = nbl)
  b[upper.tri(b)] <- "nul"
  diag(b) <- "com"
  b[nbl, nbl] <- "dnc"
  b[lower.tri(b)] <- "dnc"
  if (transformForCritFunC == TRUE) {
    r <- transform.for.critFunC(mat = b)
    return(r)
  }
  if (transformForCritFunC == FALSE) {
    return(b)
  }
}
