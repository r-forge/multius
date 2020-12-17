MARI <- function(U = U, V = V, k = 1000){
  Empirical <- MRI(U = U, V = V)
  indexes <- NULL
  for (i in 1:k){
    indexes[i] <- MRI(U = sample(U), V = sample(V))
  }
  (Empirical - mean(indexes))/(1 - mean(indexes))
}
