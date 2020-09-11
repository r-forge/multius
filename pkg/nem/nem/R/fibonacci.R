fibonacci <- function(start, n.out){
  sequence <- rep(NA, n.out)
  sequence[c(1, 2)] <- rep(start, 2)
  for (i in 3:n.out) sequence[i] <- sequence[i - 1] + sequence[i - 2]
  return(sequence)
}
