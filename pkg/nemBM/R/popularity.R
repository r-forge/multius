#' Popularity mechanism
#'
#' @description Calculate the normalized network statistic according to the popularity mechanism.
#' @name popularity
#' @param X Binary network; of class \code{matrix}.
#' @param actor Not used by the function, set to \code{NULL}. Necessary for using within other functions, e.g. nemBM.
#' @return A vector with the normalized popularity mechanism, cacluated for each unit.
#' @examples
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' popularity(X)
#' @author Marjan Cugmas and Aleš Žiberna
#' @references 
#' Cugmas, M., & Žiberna, A. (2022). Approaches to blockmodeling dynamic networks: a Monte Carlo simulation study. Social Networks, in print.
#' @export

popularity <- function(X, actor = NULL){
  popularity <- colSums(X)/nrow(X)
  relative.popularity <- popularity/sum(popularity)
  relative.popularity[is.nan(relative.popularity)] <- 0
  return(relative.popularity)
}
