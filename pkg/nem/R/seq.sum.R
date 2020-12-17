#' Generate sequnce of sums
#'
#' @description Generates a vector of values. A $i$-th value is calculated as the $i-1$ value, multiplied by $a$.
#' @param start The initial value.
#' @param n.out The number of generated values (length of a generated vector).
#' @param a what fraction of the previous value represents the current value.
#' @return A vector.
#' @author Marjan Cugmas
#' @export

seq.sum <- function(start, n.out, a){
  sequence <- rep(NA, n.out)
  sequence[1] <- start
  for (i in 2:n.out) sequence[i] <- sequence[i - 1]*a
  return(sequence)
}
