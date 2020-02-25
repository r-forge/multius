#'  Canonical correlations
#'
#' @description The function computes canonical correlations (by using \code{cc} or \code{cancor} functions) and provides with the test of
#' canonical correlations and with the eigenvalues of the canonical roots (including with the proportion of explained variances by correlation and other related
#' statistics).
#' @param x A data frame or a matrix with the values that correspond to the first set of variables (\eqn{X}-variables).
#' @param y A data frame or a matrix with the values that correspond to the second set of variables (\eqn{Y}-variables).
#' @param xcenter Wheter any centering have to be done on the \eqn{x} values before the analysis. If \code{TRUE} (default), subtract the column means. If \code{FALSE}, do not adjust the columns. Otherwise, a vector of values to be subtracted from the columns.
#' @param ycenter Analogous to \code{xcenter}, but for the \eqn{y} values.
#' @param useCCApackage Wheter \code{cc} function (from \code{CCA} package) or \code{cancor} function (from \code{stats} package) should be used to obtain canonical correlations.
#' @return
#' The function returns the same output as functions \code{cancor} or \code{cc} with the following additional results:
#' \itemize{
#' \item \code{$sigTest}
#' \itemize{
#' \item \code{WilksL} - Value of the Wilk's lambda statistic (it is a generalization of the multivariate R2; values near 0 indicate high correlation while values near 1 indicate low correlation).
#' \item \code{F} - Corresponding (to Wilk's lambda) F-ratio.
#' \item \code{df1} - Degrees of freedom for the corresponding F-ratio.
#' \item \code{df2} - Degrees of freedom for the corresponding F-ratio.
#' \item \code{p} - Probability value (p-value) for the corresponding F-ratio (Ho: The current and all the later canonical correlations equal to zero).
#' }
#' \item \code{$eigModel}
#' \itemize{
#' \item \code{Eigenvalues} - Eigenvalues of the canonical roots.
#' \item \code{\%} - Proportion of explained variance of correlation.
#' \item \code{Cum \%} - Cumulative proportion of explained variance of correlation.
#' \item \code{Cor} - Canonical correlation coeficient.
#' \item \code{Sq. Cor} - Squared canonical correlation coeficient.
#' }
#' }
#' @examples
#' cancorPlus(x = mtcars[, c(1,2,3)], y = mtcars[, c(4,5, 6)])
#' @author Aleš Žiberna, Statistical Consulting Group
#' @seealso \code{testCC}
#' @references
#' R Data Analysis Examples: Canonical Correlation Analysis, UCLA: Statistical Consulting Group. From http://www.ats.ucla.edu/stat/r/dae/canonical.htm (accessed Decembar 27, 2013).

cancorPlus<-function(x, y, xcenter = TRUE, ycenter = TRUE, useCCApackage=FALSE){
  if(useCCApackage){
    if(!require(CCA)){
      warning("CCA Package is not installed! Function cancor{stats} will be used instead!")
      cca<-cancor(x=x,y=y,xcenter=xcenter,ycenter=ycenter)
    } else {
      if(any(xcenter!=TRUE,ycenter != TRUE)) warning("xcenter and ycenter are ignored when using CCA package")
      cca<-cc(X=x,Y=y)
    }
  } else cca<-cancor(x=x,y=y,xcenter=xcenter,ycenter=ycenter)

  n<-dim(x)[1]
  p<-dim(cca$xcoef)[1]
  q<-dim(cca$ycoef)[1]
  tmp<-testCCbase(cor=cca$cor, n=n, p=p, q=q)
  res<-c(cca,tmp)
  class(res)<-class(cca)
  return(res)
}
