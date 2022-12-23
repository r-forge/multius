#' Generating symmetric networks according to the selected local network mechanisms
#'
#' @description It generates random network considering the selected local network mechanisms.
#' @param X Initial network; of class \code{matrix}.
#' @param formula The list of local netork mechanisms to be considered.
#' @param theta A vector with the mechanisms' weights/strengths.
#' @param k The number of iterations.
#' @param q The probability of establishing a link (i.e. expected/desired density).
#' @return The list with the following elements:
#' \itemize{
#' \item \code{initialNetwork} - Initial network; of class \code{matrix}.
#' \item \code{finalNetwork} - Final (generated) network; of class \code{matrix}.
#' \item \code{formula} - The list of functions that define mechanisms used.
#' \item \code{theta} - A vector with the mechanisms' weights/strengths used.
#' \item \code{k} - The number of iterations.
#' \item \code{q} - The probability of establishing a link.
#' }
#' @examples
#' formula <- list(popularity, assortativity)
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' diag(X) <- 0
#' nemSym(X = X, formula = formula, theta = c(1, 1), k = 100, q = 0.25)
#' @references  
#' Cugmas, M., DeLay, D., Žiberna, A., & Ferligoj, A. (2020). Symmetric core-cohesive blockmodel in preschool children’s interaction networks. PloS one, 15(1), e0226801.
#' @author Marjan Cugmas and Aleš Žiberna
#' @export

nemSym <- function(X, formula, theta,  k = 5000, q){
  initialNetwork <- X
  for (i in 1:k){
    actor <- sample(nrow(X), size = 1)
    l.friends <- which(X[actor,] == 1)
    nonfriends <- which(X[actor,] == 0)
    
    netStat <- WeightedNetworkStatistics(X = X, formula = formula, theta = theta, actor = actor)
    
    if (stats::rbinom(1, 1, prob = q)){
      if (length(nonfriends) > 0){
        cand <- which(netStat >= stats::quantile(netStat)[4])
        alter <- sample(cand[!cand%in%actor], 1)
        X[actor, alter] <- 1
        X[alter, actor] <- 1
      }
    }
    
    if (stats::rbinom(1, 1, prob = 1 - q)){
      if (length(l.friends) > 0){
        cand <- which(netStat <= stats::quantile(netStat)[2])
        alter <- sample(cand[!cand%in%actor], 1)
        X[actor, alter] <- 0
        X[alter, actor] <- 0
      }
    }
  }
  diag(X) <- 0
  return(list("initialNetwork" = initialNetwork,
              "finalNetwork" = X,
              "formula" = formula,
              "theta" = theta,
              "k" = k,
              "q" = q))
}
