#' Print the results of the \code{CorTestDf} function
#'
#' @description The function prints the results of the \code{CorTestDf} function.
#' @param l Output of \code{corTestDf} function.
#' @param digits Vector of length two for the number of digits (the first element of a vector corresponds to the number of digits for correlation coeficients and the second element of a vector corresponds to the number of digits for \eqn{p}-values).
#' @param format a vector of length two for the formatting of the oputput values.
#' @seealso \code{CorTestDf}
#' @examples
#' corCars <- corTestDf(mtcars[, 3:5])
#' printCorTestDf(corCars, digits = c(2, 2))
#' @author Aleš Žiberna


printCorTestDf<-function(l, digits=c(3,3), format=NULL){
  d<-dim(l$cor)
  dNames<-dimnames(l$cor)
  if(is.null(format))format<-c(sprintf("%%.%df", digits[1]), sprintf("%%.%df", digits[2]))
  l$cor<-sprintf(format[1],l$cor)	#as.character(round(l$cor,digits[1]))
  l$p<-sprintf(format[2],l$p) #as.character(round(l$p,digits[2]))
  res<-array(NA,dim=c(d,3),dimnames=c(dNames,list(c("cor","p","n"))))
  res[,,"cor"]<-l$cor
  diag(res[,,"cor"])<-""
  res[,,"p"]<-l$p
  diag(res[,,"p"])<-""
  res[,,"n"]<-l$n
  res<-as.table(res)
  res<-aperm(res,perm=c(1,3,2))
  print(ftable(res))
  invisible(res)
}

