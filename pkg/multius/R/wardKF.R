#' Calculate the value of the Ward criterion function
#'
#' @description The function calculate the value of the Ward criterion function, based on a set of numerical variables and one categorical variable (partition).
#' @param X Data frame with values of numerical variables (usually the ones that were/are used for clustering).
#' @param clu Partition.
#' @return The value of the Ward criterion function.
#' @author Aleš Žiberna
#' @export

wardKF<-function(X, clu){
  sum(by(X,INDICES=clu,FUN=ssAllVar))
}

#' @rdname wardKF
#' @export
wardCF <- wardKF
