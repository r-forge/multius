#' Plot the means
#'
#' @description The function plots the means of several numerical variables by the levels of one categorical variable.
#' @param x Data frame with values of numeric variables.
#' @param by Categorical variable that defines groups.
#' @param plotCI Wheter to plot condifence intervals or not, default is \code{TRUE}.
#' @param ylab The title of \eqn{y}-axis.
#' @param xlab The title of \eqn{x}-axis.
#' @param plotLegend Wheter to plot a legend or not, default is \code{TRUE}.
#' @param inset Inset distance(s) from the margins as a fraction of the plot region when legend is placed by keyword.
#' @param xleg Position of a legend, default is \code{topleft}.
#' @param legPar Additional parameters for a legend. They have to be provided in a list format.
#' @param gap Space left between the center of the error bar and the lines marking the error bar in units of the height (width). Defaults to 1.0
#' @param labels Labels of x-axis.
#' @param alpha A confidence level for calculating confidence intervals (default is \code{0.05}).
#' @param \dots Arguments passed to functions \code{matplot} and \code{axis}.
#' @examples
#' plotMeans(x = mtcars[, c(1, 3, 5)], by = mtcars[,8])
#' @author Aleš Žiberna

plotMeans<-function(x,
                    by,
                    plotCI = TRUE,
                    alpha = 0.05,
                    ylab = "averages",
                    xlab = "",
                    plotLegend=TRUE,
                    inset = 0.01,
                    xleg="topleft",
                    legPar=list(),
                    gap=0,
                    labels = NULL, ...){
  by <- list(by)
  tmpM<-aggregate(x = x, by = by, FUN=mean, na.rm=TRUE)
  matplot(t(tmpM[,-1]),type="o",xaxt="n", ylab=ylab, xlab = xlab,...)
  if (is.null(labels)) labels <- colnames(tmpM)[-1]
  axis(side=1,at=1:dim(tmpM[,-1])[2],las=2, labels, ...)
  g<-dim(tmpM)[1]
  res<-list(means=tmpM)
  if(plotLegend)  {
    legPar <- c(list(x=xleg,legend = tmpM[,1], pch=as.character(1:g), col=1:min(6,g),lty = 1:min(5,g),inset = inset), legPar)
    do.call(legend, args = legPar)
  }
  if(plotCI&require(gplots)){
    f<-function(x){
      n<-sum(!is.na(x))
      qt(1-alpha/2,df=n-1)*sd(x, na.rm=TRUE)/sqrt(n)
    }
    tmpCI<-aggregate(x = x,by = by, FUN=f)

    delta<-as.vector(t(tmpCI[,-1]))
    y<-as.vector(t(tmpM[,-1]))
    m<-dim(tmpM)[2]-1
    x<-rep(1:m, times=g)
    cols<-rep(1:g, each=m)
    plotCI(x=x,y=y,uiw = delta, add=TRUE, labels=TRUE, gap=gap, col=cols,type="n")
    res<-c(res, list(CI=tmpCI))
  }
  invisible(res)
}
