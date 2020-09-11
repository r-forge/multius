dens.plot <- function(M, clu, type = "ave", ...){
  dens <- matrix(NA, nrow = max(clu), ncol = max(clu))
  for (i in 1:max(clu)){
    for (j in 1:max(clu)){
      dens[i, j] <- mean(M[clu == i, clu == j])
    }
  }
  if (type == "emp") plot.mat(dens, maxValPlot = 1, ...)
  if (type == "ave") plot.mat(dens > mean(M), ...)
  if (type == "cla") plot.mat(M = M, clu = clu, ...)
}
