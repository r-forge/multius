sq.diff <- function(covariate, actor){
  v <- prop.table((covariate - covariate[actor])**2)
  v[is.nan(v)] <- 0
  return(v)
}
