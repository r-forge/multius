#' Sum of squared error across blocks
#'
#' @description It calculates the sum of square differences between the desired (specified by an image matrix M) denstities and empirical densities. 
#' @param X Initial binary network; of class \code{matrix}.
#' @param M Image matrix with block densities.
#' @param partition A partition in a vector format. Each unique value (positive integers) represents one cluster.
#' @param loops Wheter loops are allowed or not.
#' @return Sum of squared error (a single value).
#' @examples
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' diag(X) <- 0
#' M <- matrix(c(0.1, 0.4, 0.5, 0.3), nrow = 2)
#' partition <- c(1, 2, 2, 1, 1, 2, 2, 2, 1)
#' SSEblock(X = X, M = M, partition = partition, loops = TRUE)
#' @author Marjan Cugmas
#' @import blockmodeling
#' @export

SSEblock <- function(X, M, partition, loops) {
  return(sum((funByBlocks(M = X, clu = partition, FUN = mean, ignore.diag = !loops) - M)**2))
}


