#' Simple version of omega coefficient - measure of measurement internal consistency based on factor analysis
#'
#' @description The function omega coefficient, which is a measure of measurement internal consistency based on factor analysis, based on the covariance or correlation matrix. \code{\link[psych:fa]{psych::fa}} is used to preform factor analysis.
#' @param C Covariance or correlation matrix.
#' @param fm Factor analysis method, maximum likelihood (\code{"ml"}) by default. See \code{\link[psych:fa]{psych::fa}} for details. Only used if \code{usePsych} is \code{TRUE} and \code{psych} package is available.
#' @param nfactors Number of factors, 1 by default, \code{\link[psych:fa]{psych::fa}} for details.
#' @param covar Should the input \code{C} be treated as covariance matrix. Defaults to \code{TRUE}. If set to \code{FALSE}, the input \code{C} is converted to correlation matrix using \code{\link[stats:cov2cor]{stats::cov2cor}}.
#' @param usePsych Should \code{psych} package or more precisely \code{\link[psych:fa]{psych::fa}} be used to perform factor analysis. Defaults to \code{TRUE}. If \code{FALSE} or \code{psych} package is not available, \code{\link[stats:factanal]{stats::factanal}} is used.
#' @param returnFaRes Should results of factor analysis be returned in addition to the computed omega coefficient. \code{FALSE} by default.
#' @param rotation Rotation to be used in factor analysis. Defaults to "none", as it does not influence the Omega coefficient. Used only if \code{returnFaRes} is \code{TRUE}. Included if one wants to customize the results of factor analyisis. See \code{\link[psych:fa]{psych::fa}} or \code{\link[stats:factanal]{stats::factanal}}  for details (depending on which function is used, see \code{usePsych}).
#' @param \dots Additional parameters to \code{\link[psych:fa]{psych::fa}} or \code{\link[stats:factanal]{stats::factanal}} (depending on which function is used, see \code{usePsych}).
#' @return
#' By default just the value of the omega coefficient. If \code{returnFaRes} is \code{TRUE}, then a list with two elements:
#' \itemize{
#' \item \code{omega} - The value of the omega coefficient.
#' \item \code{faRes} - The result of factor analysis.
#' }
#' @examples
#' Omega(C=cor(mtcars[,1:6]),nfactors=1)
#' Omega(C=cor(mtcars[,1:6]),nfactors=1,returnFaRes=TRUE)
#' @author Ales Ziberna
#' @export


Omega<-function(C, fm="ml", nfactors=1, covar=TRUE, usePsych=TRUE, returnFaRes=FALSE, rotation="none", ...){
  if(covar==FALSE) C<-stats::cov2cor(C)
  if(!returnFaRes) rotation<-"none"
  if(usePsych&&requireNamespace("psych", quietly = TRUE)){
    res<-psych::fa(r=C,fm=fm, nfactors = nfactors, rotate=rotation, covar = TRUE, ...)
    communality<-res$communality
  } else{
    res<-stats::factanal(covmat = C, factors = nfactors, rotation = rotation, ...)
    communality<-diag(C)-res$uniquenesses
  }

  omega <- 1 - (sum(diag(C))-sum(communality))/sum(C)
  if(returnFaRes) {
    return(list(omega=omega, faRes=res))
  } else return(omega)
}

