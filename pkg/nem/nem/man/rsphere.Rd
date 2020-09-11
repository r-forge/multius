\name{rsphere}
\alias{rsphere}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating random values on a sphere
}
\description{
It generates random values on a n-dimensional sphere.
}
\usage{
rsphere(n = 1000, dim = 3, r = 1, pos.only = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{the number of units}
  \item{dim}{the number of variables}
  \item{r}{the diameter of a sphere}
  \item{pos.only}{if \code{TRUE} then it generates only positive values}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a data frame with \code{n} rows and \code{dim} columns.
}
\references{
Muller 1959, Marsaglia 1972
}
\author{
Marjan Cugmas
}

