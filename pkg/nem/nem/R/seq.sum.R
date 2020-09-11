seq.sum <- function(start, n.out, power){
  sequence <- rep(NA, n.out)
  sequence[1] <- start
  for (i in 2:n.out) sequence[i] <- sequence[i - 1]*power
  return(sequence)
}

