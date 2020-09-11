check.convergence <- function(x, accmean = 0.01, accstdev = 0.01, w = 0.3, mini = 200, maxi = 300){
  if ((length(x) > mini) & (length(x) < maxi)) {
    first.ones <- c(1:floor(((1-w)*length(x))))
    cond.mean <- (abs(mean(x) - mean(x[first.ones]))/mean(x)) <= accmean
    cond.stdev <- (abs(sd(x) - sd(x[first.ones]))/sd(x)) <= accstdev
    return(cond.mean == TRUE & cond.stdev == TRUE)
  } else if (length(x) > maxi) {return(TRUE)} else return(FALSE)
}
