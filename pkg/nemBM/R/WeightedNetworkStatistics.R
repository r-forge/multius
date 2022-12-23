#' Weighted network statistics
#'
#' @description It calculates the weighted network statistics, considering the selected local network mecahnisms and their weights.
#' @param X Binary network; of class \code{matrix}.
#' @param formula The list of local netork mechanisms to be considered.
#' @param theta A vector with the mechanisms' weights/strengths.
#' @param actor A unit (actor; row/column number), which have an opportunity to change a link.
#' @param randomSD The srandard deviation of a normal distribution form which the random part of weighed network statistics is sampled.
#' @return The data frame with one column and the number of rows equal to the number of units.
#' @examples
#' formula <- list(mutuality, popularity, OTPtransitivity)
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' WeightedNetworkStatistics(X = X, formula = formula, theta = c(1, 1, 1), actor = 1)
#' @author Marjan Cugmas and Aleš Žiberna
#' @export

WeightedNetworkStatistics <- function(X, formula, theta, actor, randomSD = 0){
  if (is.null(formula)){
    netStat <- matrix(1, nrow = nrow(X))
  } else {
    netStat00 <- sapply(X = 1:length(formula), FUN = function(i){formula[[i]](X, actor = actor)})
    netStat00[is.nan(netStat00)] <- 0
    netStat <- netStat00 %*% theta
    netStat <- netStat + stats::rnorm(n = nrow(X), mean = 0, sd = randomSD)
  }
  rownames(netStat) <- rownames(X)
  return(netStat)
}
