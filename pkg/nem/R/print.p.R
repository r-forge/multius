print.p <- function(p){
  if (p<0.01) return("< 0.01")
  if (p<0.05) return("< 0.05")
  if (p<0.10) return("< 0.10")
  else return(paste("= ", round(p, 2)))
}
