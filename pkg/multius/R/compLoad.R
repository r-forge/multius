#' Compare factor loadings
#'
#' @description The function compares two sets of factor loadings by considering different possible orders of factors and different possible signs of factor loadings.
#' @param L1 First set of factor loadings in a matrix form (variables are organized in rows and factors are organized in columns).
#' @param L2 Second set of factor loadings in a matrix form (variables are organized in rows and factors are organized in columns).
#' @return A list with the following elements:
#' \itemize{
#' \item \code{err} - Sum of squared differences between the values of \code{L1} and \code{L2} (for the corresponding permuation and signs).
#' \item \code{perm} - Permutation of columns of \code{L1} that results in the lowest \code{err} value.
#' \item \code{sign} - Signs of factor loadings of \code{L1}. The first value corresponds to the first column of \code{L1} and the second value corresponds to the second column of \code{L1}.
#' }
#' @examples
#' L1 <- cbind(c(0.72, 0.81, 0.92, 0.31, 0.22, 0.15), c(0.11, 0.09, 0.17, 0.77, 0.66, 0.89))
#' L2 <- cbind(c(-0.13, -0.08, -0.20, -0.78, -0.69, -0.88), c(0.72, 0.82, 0.90, 0.29, 0.20, 0.17))
#' compLoad(L1, L2)
#' @author Aleš Žiberna and Friedrich Leisch (permutations)
#' @export

compLoad <- function(L1, L2){
  nF <- dim(L1)[2]

  permutations <- function (n) {
    if (n == 1)
      return(matrix(1))
    else if (n < 2)
      stop("n must be a positive integer")
    z <- matrix(1)
    for (i in 2:n) {
      x <- cbind(z, i)
      a <- c(1:i, 1:(i - 1))
      z <- matrix(0, ncol = ncol(x), nrow = i * nrow(x))
      z[1:nrow(x), ] <- x
      for (j in 2:i - 1) {
        z[j * nrow(x) + 1:nrow(x), ] <- x[, a[1:i + j]]
      }
    }
    dimnames(z) <- NULL
    z
  }

  allPerm <- permutations(nF)
  err <- Inf
  bestPerm <- NA
  for(j in 1:dim(allPerm)[1]){
    L2Per <- L2[,allPerm[j,]]
    # vsota kvadratov napak za pozitivne in negativne predznake
    e1 <- apply((L1-L2Per)^2,2,sum)
    e2 <- apply((L1+L2Per)^2,2,sum)
    # izbere manjso vrednost napak
    minErr <- pmin(e1,e2)
    # vsota napak
    terr <- sum(pmin(e1,e2))
    if(err > terr) {
      err <- terr
      bestPerm <- allPerm[j,]
      sign <- (minErr==e1)*2-1
    }
  }
  return(list(err=err, perm=bestPerm, sign=sign))
}


