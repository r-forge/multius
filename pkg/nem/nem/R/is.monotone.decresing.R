is.monotone.decresing <- function(x){
  ifelse(sum(diff(rev(x)) < 0) == 0, yes = TRUE, no = FALSE)
}
