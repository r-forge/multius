#' Sum of squared error across blocks
#'
#' @description The actor choose the block (i.e., column in an image matrix) in which he will change a link, based on the difference between the density of his out-degrees by blocks and the ideal block density.
#' @param X Binary network; of class \code{matrix}.
#' @param actor A unit (actor; row/column number), which have an opportunity to change a link.
#' @param M Image matrix with block densities.
#' @param partition A partition in a vector format. Each unique value (positive integers) represents one cluster.
#' @param loops Wheter loops are allowed or not.
#' @param randomBlock How to select a block; the one with the highest difference (\code{FALSE}, default), proportionally to the differences (\code{linear}) or squared differences (\code{square}).
#' @return A vector with two elements: \code{block} (selected block number) and \code{sign} (wheter the selected block is too sparse (-1) or too dense (+1)).
#' @examples
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' diag(X) <- 0
#' M <- matrix(c(0.1, 0.4, 0.5, 0.3), nrow = 2)
#' partition <- c(1, 2, 2, 1, 1, 2, 2, 2, 1)
#' chooseBlockRow(X = X, actor = 3, partition = partition, 
#' M = M, loops = FALSE, randomBlock = "square")
#' @author Marjan Cugmas
#' @export

chooseBlockRow <- function(X, actor, partition, M, loops, randomBlock = FALSE){
  if (loops) gostoteR <- as.vector(by(X[actor, ], partition, mean)) else gostoteR <- as.vector(by(X[actor, -actor], partition[-actor], mean))
  napakeR <- gostoteR - M[partition[actor],]
  
  # vsakemu bloku dodamo eno napako
  predznaki <- sign(napakeR)
  
  if (randomBlock == FALSE){
    kandidati <- which(max(napakeR) == napakeR)
    if (length(kandidati) == 1) block <- kandidati else block <- sample(kandidati, size = 1)
  }
  
  napakeR <- abs(napakeR) + 1/table(partition)
  if (randomBlock == "square"){
    block <- sample(1:nrow(M), size = 1, prob = prop.table(napakeR**2))
  }
  if (randomBlock == "linear"){
    block <- sample(1:nrow(M), size = 1, prob = prop.table(napakeR))
  }
  return(c("block" = as.numeric(block), "sign" = as.numeric(predznaki[block])))
}
