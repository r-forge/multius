#' Modified Rand Index (MRI) and Modified Adjusted Rand Index (MARI)
#'
#' @description It computes the value of the adjuster or non-adjusted Modified Rand Index.
#' @param U The first partition.
#' @param V The second partition.
#' @param k the number of iterations to estimate the expected value in the case of two random and independent partitions.
#' @details The corresponding values for the first and/or second partitions for in-comers and/or out-goings have to be NA.
#' @return A single value.
#' @references Cugmas, M., & Ferligoj, A. (2018). Comparing two partitions of non-equal sets of units. Advances in Methodology and Statistics 1(15), 1-23.
#' @author Marjan Cugmas
#' @export

MARI <- function(U = U, V = V, k = 1000){
  Empirical <- MRI(U = U, V = V)
  indexes <- NULL
  for (i in 1:k){
    indexes[i] <- MRI(U = sample(U), V = sample(V))
  }
  (Empirical - mean(indexes))/(1 - mean(indexes))
}
