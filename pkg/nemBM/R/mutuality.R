#' Mutuality mechanism
#'
#' @description Calculate the normalized network statistic according to the mutuality mechanism.
#' @name mutuality
#' @param X Binary network; of class \code{matrix}.
#' @param actor A unit (actor; row/column number), which have an opportunity to change a link.
#' @return A vector with the normalized mutuality mechanism, cacluated between the actor and other units.
#' @examples
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' mutuality(X, actor = 2)
#' @author Marjan Cugmas and Aleš Žiberna
#' @references 
#' \itemize{
#' \item Cugmas, M., Žiberna, A., & Ferligoj, A. (2019). Mechanisms generating asymmetric core-cohesive blockmodels. Advances in Methodology and Statistics, 16(1), 17-41.
#' \item Cugmas, M., & Žiberna, A. (2022). Approaches to blockmodeling dynamic networks: a Monte Carlo simulation study. Social Networks, in print.
#' }
#' @export

mutuality <- function(X, actor){
  asyminties <- as.numeric(X[, actor] + (X[actor, ]*-1) == 1)
  asyminties.relative <- asyminties/sum(asyminties)
  asyminties.relative[is.nan(asyminties.relative)] <- 0
  return(asyminties.relative)
}
