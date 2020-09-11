plot.bm <- function(rez = NULL, clu = NULL, im = NULL, useRez = FALSE, ...){
  if (useRez == FALSE){
    im <- im[,,]
    im[im == "nul"] <- 0
    im[im == "com"] <- 1
  }
  if (useRez == TRUE){
    im <- IM(rez)[, , ]
    im[im == "nul"] <- 0
    im[im == "com"] <- 1
    clu <- clu(rez)
  }
  plot.network(as.network(im, loops = TRUE), vertex.cex = sqrt(prop.table(table(clu))/pi) * 10, ...)
}
