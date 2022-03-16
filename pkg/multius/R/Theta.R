#' Theta coefficient - measure of measurement internal consistency based on principal component analysis
#'
#' @description The function theta coefficient, which is a measure of measurement internal consistency based on principal component analysis, or more precisely first eigenvalue.
#' @param C Covariance or correlation matrix.
#' @return
#' The value of the theta coefficient.
#' @examples
#' Theta(C=cor(mtcars[,1:6]))
#' @author Ales Ziberna
#' @export

Theta<-function(C){
  PC<-stats::princomp(covmat = C)
  l1<-PC$sdev[1]^2
  N<-nrow(C)
  theta<-N/(N-1)*(1-1/l1)
  return(theta)
}
