#' Transform continous variable to a Likert-type variable
#'
#' @description The function transforms a continous variable to a \eqn{k}-point Likert-type variable. Different styles of answering to a survey are possible.
#' @param x Vector with values to be transformed.
#' @param type Type of transformation. Possible values are: \code{eq} (default) (equal wide intervals), \code{yes} (wider intervals at higher values of \code{x}), \code{no} (wider intervals at lower values of \code{x}), \code{avg} (wider intervals near the mean of \code{x}).
#' @param k Number of classes.
#' @param q Extension factor. Tells how much is each next interval wider then the previous one. Not used when \code{type="eq"}.
#' @param r Minimum and maximum values to define intervals of \code{x}. Default are minimum and maximum values of \code{x}.
#' @param num If \code{TRUE} (default) numberical values are returned, otherwise intervals are returned.
#' @return Transformed values are organized into a vector.
#' @examples
#' x <- rnorm(1000)
#' hist(x = toLik(x, type = "eq"), breaks = 0:5+0.5, xlab = "answer", main = "type = 'eq'")
#' hist(x = toLik(x, type = "yes"), breaks = 0:5+0.5, xlab = "answer", main = "type = 'yes'")
#' hist(x = toLik(x, type = "no"), breaks = 0:5+0.5, xlab = "answer", main = "type = 'no'")
#' hist(x = toLik(x, type = "avg"), breaks = 0:5+0.5, xlab = "answer", main = "type = 'avg'")
#' @author Aleš Žiberna

toLik<-function(x,type="eq",q=1.5,k=5,r=range(x), num=TRUE){
  d<-diff(r)
  if(type=="eq"){
    br<-c(-Inf,r[1]+1:(k-1)*d/k,Inf)
  }else if(type=="yes"){
    int <- d/sum(q^(c(0:(k-1))))
    br<-c(-Inf,r[1]+cumsum(q^(0:(k-2)))*int,Inf)
  }else if(type=="no"){
    q<-1/q
    int <- d/sum(q^(c(0:(k-1))))
    br<-c(-Inf,r[1]+cumsum(q^(0:(k-2)))*int,Inf)
  }else if(type=="avg"){
    q<-1/q
    br<-numeric(k+1)
    br[1]<- -Inf
    br[k+1]<- Inf
    if(k%%2==0){
      int <- d/sum(2*q^(c(0:(k/2-1))))
      br[k/2+1] <- (r[1]+r[2])/2
      for (i in c(1:(k/2-1))){
        br[k/2+1+i] <- br[k/2+1+i-1]+int*q^(i-1)
        br[k/2+1-i] <- br[k/2+1-i+1]-int*q^(i-1)}
    } else {
      int <- d/sum(1,2*q^(c(1:((k-1)/2))))
      center <- (r[1]+r[2])/2
      br[k/2+1.5] <- center+int/2
      br[k/2+0.5] <- center-int/2
      # dodati zaradi robnega pogoja
      if (k > 3){
        for (i in c(1:((k-3)/2))){
          br[k/2 + 1.5 + i] <- br[k/2+1.5 + i - 1]+int*q^i
          br[k/2 + 0.5 - i] <- br[k/2+0.5 - i + 1]-int*q^i
        }
      }
    }
  }
  xd<-cut(x,breaks=br)
  if(num) xd<-as.numeric(xd)
  return(xd)
}
