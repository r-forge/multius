#' Histogram with normal curve
#'
#' @description The function draws a histogram with a normal density curve. The parameters (mean and standard deviation) are estimated on the empirical data.
#' @param y A vector of observations.
#' @param freq Wheter frequencies (\code{freq = TRUE}) of density (\code{freq = FALSE}) should be represented on \eqn{y}-axis.
#' @param breaks See help file for function \code{hist}.
#' @param \dots Arguments passed to function \code{hist}.
#' @return A list with two elements:
#' \itemize{
#' \item \code{x} - breaks, see \code{graphics::hist}.
#' \item \code{y} - frequencies or relative frequencies, see \code{graphics::hist}.
#' }
#' @examples
#' histNorm(rnorm(1000), freq = TRUE)
#' histNorm(rnorm(1000), freq = FALSE)
#' @author Marjan Cugmas
#' @export

histNorm <- function (y, breaks = "Sturges", freq = TRUE, ...) {
  y <- stats::na.omit(y)
  h <- graphics::hist(y, freq = freq, breaks, ...)
  x <- NULL
  if (!h$equidist & freq) {
    warning("Normal distribution not added due to using unequal bin widths with frequencies!")
  }
  else graphics::curve(stats::dnorm(x, mean = mean(y), sd = stats::sd(y)) * ifelse(freq == TRUE, yes = sum(!is.na(y)) * diff(h$breaks)[1], no = 1),
             add = TRUE, col = "red", lwd = 2, lty = 2, xpd = TRUE)
}

