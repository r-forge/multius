#' Generating networks according to the selected local network mechanisms
#'
#' @description It generates random network considering the selected local network mechanisms.
#' @param X Initial network; of class \code{matrix}.
#' @param formula The list of local netork mechanisms to be considered.
#' @param theta A vector with the mechanisms' weights/strengths.
#' @param k The number of iterations.
#' @param q The probability of establishing a link.
#' @param b The share of alters among which an actor (i.e., ego) chooses to create or break a tie.
#' @return The list with the following elements:
#' \itemize{
#' \item \code{initialNetwork} - Initial network; of class \code{matrix}.
#' \item \code{finalNetwork} - Final (generated) network; of class \code{matrix}.
#' \item \code{formula} - The list of functions that define mechanisms used.
#' \item \code{theta} - A vector with the mechanisms' weights/strengths used.
#' \item \code{k} - The number of iterations.
#' \item \code{q} - The probability of establishing a link.
#' \item \code{b} - The share of alters among which an actor (i.e., ego) chooses to create or break a tie.
#' }
#' @examples
#' formula <- list(mutuality, popularity, assortativity)
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' nem(X = X, formula = formula, theta = c(1, 1, 1), k = 100, q = 0.25)
#' @references  
#' Cugmas, M., Žiberna, A., & Ferligoj, A. (2019). Mechanisms generating asymmetric core-cohesive blockmodels. Advances in Methodology and Statistics, 16(1), 17-41.
#' @author Marjan Cugmas and Aleš Žiberna
#' @export

nem <- function(X, formula, theta,  k = 5000, q, b = 0.25){
  initialNetwork <- X
  for (i in 1:k){
    actor <- sample(nrow(X), size = 1)
    l.friends <- which(X[actor,] == 1)
    nonfriends <- which(X[actor,] == 0)
    
    netStat <- WeightedNetworkStatistics(X = X, formula = formula, theta = theta, actor = actor, randomSD = 0)
    
    urej  <- cbind(1:nrow(X), order(netStat), netStat)[-actor,]
    
    if(sample(c(0,1), prob = c(1-q, q), 1) == 1){
      if (length(nonfriends) > 0){
        X[actor, sample(urej[which(urej[,3] >= stats::quantile(urej[,3], probs = 1-b)), 1], 1)] <- 1
      }
    }
    
    if(sample(c(0,1), prob = c(1-q, q), 1) == 0){
      if (length(l.friends) > 0){
        X[actor, sample(urej[which(urej[,3] <= stats::quantile(urej[,3], probs = b)), 1], 1)] <- 0
      }
    }
  }
  return(list("initialNetwork" = initialNetwork,
              "finalNetwork" = X,
              "formula" = formula,
              "theta" = theta,
              "k" = k,
              "q" = q,
              "b" = b))
}
