#' @rdname corTestDf
#' @param x Output of \code{corTestDf} function.
#' @param digits Vector of length two for the number of digits (the first element of a vector corresponds to the number of digits for correlation coefficients and the second element of a vector corresponds to the number of digits for \eqn{p}-values).
#' @param format A vector of length two for the formatting of the output values.
#' @param \dots Other parameters to print.default (not needed).
#' @method print corTestDf
#' @export

print.corTestDf <- function(x, digits=c(3,3), format=NULL, ...){
  #digits=c(3,3), format=NULL,
#  digits=c(3,3); format=NULL
  d<-dim(x$cor)
  dNames<-dimnames(x$cor)
  if(is.null(format))format<-c(sprintf("%%.%df", digits[1]), sprintf("%%.%df", digits[2]))
  x$cor<-sprintf(format[1],x$cor)	#as.character(round(x$cor,digits[1]))
  x$p<-sprintf(format[2],x$p) #as.character(round(x$p,digits[2]))
  res<-array(NA,dim=c(d,3),dimnames=c(dNames,list(c("cor","p","n"))))
  res[,,"cor"]<-x$cor
  diag(res[,,"cor"])<-""
  res[,,"p"]<-x$p
  diag(res[,,"p"])<-""
  res[,,"n"]<-x$n
  res<-as.table(res)
  res<-aperm(res,perm=c(1,3,2))
  print(stats::ftable(res),...)
  invisible(res)
}

#' @rdname corTestDf
#' @param l Output of \code{corTestDf} function.
#' @export
printCorTestDf<-function(l, digits=c(3,3), format=NULL, ...){
  print.corTestDf(x=l, digits = digits, format = format, ...)
}

