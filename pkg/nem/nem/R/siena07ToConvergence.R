siena07ToConvergence <- function(alg, data, effects, ans0=NULL, ...){
  numr <- 0
  ans <- siena07(alg, data=data, effects=effects, prevAns=ans0, ...) # the first run
  repeat {
    numr <- numr+1 # count number of repeated runs
    tm <- ans$tconv.max # convergence indicator
    cat(numr, tm,"\n") # report how far we are
    if (tm < 0.25) {break} # success
    if (tm > 10) {break} # divergence without much hope
    # of returning to good parameter values
    if (numr > 30) {break} # now it has lasted too long
    ans <- siena07(alg, data=data, effects=effects, prevAns=ans, ...)
  }
  if (tm > 0.25)
  {
    cat("Warning: convergence inadequate.\n")
  }
  ans
}
