ccps <- function (nbl) {
  B.new <- array(NA, dim = c(1, 1, nbl, nbl))
  B.new[1, 1, , ] <- matrix("null", nrow = nbl, ncol = nbl)
  diag(B.new[1, 1, , ]) <- "com"
  B.new[1, 1, , ][, 1] <- "com"
  B.new[1, 1, , ][1, ] <- "com"
  return(B.new)
}

