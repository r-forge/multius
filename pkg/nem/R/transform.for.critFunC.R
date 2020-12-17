transform.for.critFunC <- function(mat){
  d <- a <- mat
  nbl <- nrow(mat)
  a[a == "dnc"] <- "com"
  d[d != "dnc"] <- NA
  d[d == "dnc"] <- "nul"
  r <- array(NA, dim = c(2, 1, nbl, nbl))
  r[1, 1,,] <- a
  r[2, 1,,] <- d
  return(r)
}
