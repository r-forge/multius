#' Test of canonical correlations
#'
#' @description The function perform the Wilk's test for the statistical significance of canonical correlations.
#' @param cor Vector with canonical correlations.
#' @param n Number of units.
#' @param p Number of variables in the first group of variables.
#' @param q Number of variables in the second group of variables.
#' @return The results are organized in a \code{list} format with two data tables:
#'
#' \code{sigTest}
#' \itemize{
#' \item \code{WilksL} - Value of the Wilk's lambda statistic (it is a generalization of the multivariate R2; values near 0 indicate high correlation while values near 1 indicate low correlation).
#' \item \code{F} - Corresponding (to Wilk's lambda) F-ratio.
#' \item \code{df1} - Degrees of freedom for the corresponding F-ratio.
#' \item \code{df2} - Degrees of freedom for the corresponding F-ratio.
#' \item \code{p} - Probability value (p-value) for the corresponding F-ratio (Ho: The current and all the later canonical correlations equal to zero).
#' }
#' \code{eigModel}
#' \itemize{
#' \item \code{Eigenvalues} - Eigenvalues of the canonical roots.
#' \item \code{\%} - Proportion of explained variance of correlation.
#' \item \code{Cum \%} - Cumulative proportion of explained variance of correlation.
#' \item \code{Cor} - Canonical correlation coeficient.
#' \item \code{Sq. Cor} - Squared canonical correlation coeficient.
#' }
#' @examples
#' testCC(cor = c(0.76, 0.51, 0.35, 0.28, 0.10), n = 51, p = 5, q = 5)
#' @author Aleš Žiberna
#' @references
#' R Data Analysis Examples: Canonical Correlation Analysis, UCLA: Statistical Consulting Group. From http://www.ats.ucla.edu/stat/r/dae/canonical.htm (accessed Decembar 27, 2013).

# testCC<-function(cancor, n){
#   testCCbase(cor=cancor$cor, n, p=dim(cancor$xcoef)[1], q=dim(cancor$ycoef)[1])
# }

testCC<-function(cor, n, p, q){
  testCCbase(cor=cor, n, p=p, q=q)
}
