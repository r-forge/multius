#' Randomize a partition
#'
#' @description It randomizes a partition by randomly relocating a given share of units between the clusters. The group sizes are preserved.
#' @param partition Initial partition in a vector format. Each unique value (positive integers) represents one cluster.
#' @param p The share of relocated units.
#' @param checkSelected If \code{TRUE} (default is \code{FALSE}) a given unit can be relocated only once.
#' @return A partition (in a vector format).
#' @examples
#' randomizePartition(partition = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), p = 0.3)
#' @author Marjan Cugmas and Aleš Žiberna
#' @export

randomizePartition <- function(partition, p, checkSelected = FALSE){
  n <- length(partition)
  nPre <- n*p/2
  s <- floor(nPre) + stats::rbinom(n = 1, size = 1, prob = nPre - trunc(nPre))
  
  if (checkSelected){
    if ((p > 0) & (s > 0)) {
      zeIzbrani <- NULL
      for (zamenjave in 1:s){
        if (!is.null(zeIzbrani)) i <- sample(c(1:n)[-zeIzbrani], 1) else i <- sample(1:n, 1)
        zeIzbrani[zamenjave] <- i
        
        if (!is.null(zeIzbrani)) j <- sample(c(1:n)[-c(which(partition %in% partition[i]), zeIzbrani)], 1) else j <- sample(c(1:n)[-which(partition %in% partition[i])], 1)
        zeIzbrani[zamenjave] <- j
        
        tmp <- partition
        tmp[i] <- partition[j]
        tmp[j] <- partition[i]
        partition <- tmp
      }
    }
  } else {
    if ((p > 0) & (s > 0)) {
      for (zamenjave in 1:s){
        pogoj <- TRUE
        while(pogoj){
          i <- sample(1:n, 1)
          j <- sample(c(1:n)[-which(partition %in% partition[i])], 1)
          pogoj <- j==i
        }
        
        tmp <- partition
        tmp[i] <- partition[j]
        tmp[j] <- partition[i]
        partition <- tmp
      }
    }
  }
  return(partition)
}
