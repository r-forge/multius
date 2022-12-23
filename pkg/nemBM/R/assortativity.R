#' Assortativity mechanism
#'
#' @description Calculate the normalized network statistic according to the assortativity mechanism.
#' @name assortativity
#' @param X Binary network; of class \code{matrix}.
#' @param actor A unit (actor; row/column number), which have an opportunity to change a link.
#' @return A vector with the assortativity mechanism, cacluated between the actor and other units.
#' @details The function returns the value 1 when actor (i.e. ego) and alter do not differ in the number of incoming ties. 
#' Otherwise, lower values indicate higher difference in the number of incoming ties between the actor and alter.
#' @examples
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' mutuality(X, actor = 2)
#' @author Marjan Cugmas and Aleš Žiberna
#' @references 
#' Cugmas, M., & Žiberna, A. (2022). Approaches to blockmodeling dynamic networks: a Monte Carlo simulation study. Social Networks, in print
#' @export

assortativity<-function (X, actor){
  diff <- abs(sum(X[, actor]) - colSums(X))^2
  max.deg <- apply(cbind(colSums(X), rep(sum(X[, actor]), nrow(X))), 1, max)
  all <- 1 - diff/4
  all<-ifelse(all<0,0,all)
  return(all)
}

