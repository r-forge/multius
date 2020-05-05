#' Anti-image matrix
#'
#' @description The function computes anti-image matrix (i.e., with partial correlations on the off-diagonal and with KMO-MSAs on the diagonal) and the overall KMO.
#' @param X A data frame with the values of numerical variables.
#' @return
#' \itemize{
#' \item \code{AIR} - Anti-image matrix.
#' \item \code{KMO} - Overall KMO.
#' }
#' @examples
#' antiImage(X = mtcars[, c(1, 3, 4, 5)])
#' @author
#' Marjan Cugmas
#' @references Kaiser, H. F., & Rice, J. (1974). Little Jiffy, Mark Iv. Educational & Psychological Measurement, 34(1), 111.

antiImage <- function(X){
  varNames <- colnames(X)
  R <- cor(as.matrix(X))
  iR <- solve(R)
  AIR <- diag((diag(iR)**(-1/2))) %*% iR %*% diag(((diag(iR)**(-1/2))))
  KMO <- (sum(R**2)-nrow(R))/(sum(R**2+AIR**2) - nrow(R)*2)
  diag(AIR) <- (colSums(R**2) - 1)/(colSums(R**2+AIR**2) - 2)
  rownames(AIR) <- colnames(AIR) <- varNames
  return(list("AIR" = AIR, "KMO" = KMO))
}

