% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.chitest.R
\name{report.chitest}
\alias{report.chitest}
\title{Report the results of chi-square test}
\usage{
report.chitest(
  varNamesX,
  varNamesY,
  data,
  simulate.p.value = TRUE,
  cramer = FALSE,
  language = "english",
  ...
)
}
\arguments{
\item{varNamesX}{A vector of strings representing the names of the X variables in the data frame.}

\item{varNamesY}{A vector of strings representing the names of the Y variables in the data frame.}

\item{data}{A data frame or an object of class \code{survey.design} from \code{survey} package.}

\item{simulate.p.value}{A logical value indicating whether to compute p-values by Monte Carlo simulation. Default is TRUE.}

\item{cramer}{A logical value indicating whether to compute Cramer's V for the pairs. Default is FALSE.}

\item{language}{The language used for displaying the statistics in the frequency table. This parameter accepts two values: \code{english} or \code{slovene}. Depending on the chosen language, all statistical terms and output will be adjusted accordingly. Default is \code{english}.}

\item{...}{Additional arguments to be passed to the chisq.test function.}
}
\value{
A matrix with rows representing each pair of variables, and columns representing:
        1) variable pair,
        2) Chi-square statistic,
        3) p-value (calculated by simulation or classic method, based on 'simulate.p.value'),
        4) degrees of freedom (if 'simulate.p.value' is FALSE),
        5) Cramer's V (if 'cramer' is TRUE).
}
\description{
This function conducts Chi-square tests for independence between pairs of categorical variables.
It provides the option to use Monte Carlo simulations for the p-value computation and to calculate Cramer's V.
}
\examples{
report.chitest(varNamesX = c("cyl", "vs", "am"),
varNamesY = "gear", data = mtcars, simulate.p.value = TRUE)
}
\author{
Marjan Cugmas
}
