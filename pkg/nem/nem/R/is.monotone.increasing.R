is.monotone.increasing <- function(x) {
  ifelse(sum(diff(x) < 0) == 0, yes = TRUE, no = FALSE)
}
