#' Outgoing shared partners mechanism
#'
#' @description Calculates the network statistic according to the outgoing shared partners mechanism.
#' @name OSPtransitivity
#' @param X Binary network; of class \code{matrix}.
#' @param actor A unit (actor; row/column number), which have an opportunity to change a link.
#' @return A vector with the number of paths of length two between the actor and other units.
#' @examples
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' OSPtransitivity(X, actor = 2)
#' @author Marjan Cugmas and Aleš Žiberna
#' @references 
#' Cugmas, M., & Žiberna, A. (2022). Approaches to blockmodeling dynamic networks: a Monte Carlo simulation study. Social Networks, in review.
#' @export

OSPtransitivity <- function (X, actor) {
  counts <- NULL
  for (i in 1:nrow(X)) {
    friends <- cbind(X[actor, ], X[i, ])
    counts[i] <- sum(rowSums(friends) == 2)
  }
  counts[actor] <- 0
  if (sum(counts) != 0) counts <- counts/sum(counts)
  return(counts)
}

