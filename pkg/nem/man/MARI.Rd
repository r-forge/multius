\name{MARI}
\alias{MARI}
\alias{MRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Rand Index (MRI) and Modified Adjusted Rand Index (MARI)
}
\description{
It computes the value of the adjuster or non-adjusted Modified Rand Index.
}
\usage{
MRI(U = U, V = V, k = 1000)
MARI(U = U, V = V, k = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{U}{the first partition}
  \item{V}{the second partition}
  \item{k}{the number of iterations to estimate the expected value in the case of two random and independent partitions}
}
\details{
The corresponding values for the first and/or second partitions for in-comers and/or out-goings have to be NA.
}
\value{
It returns a single value.
}
\references{
Cugmas, M., & Ferligoj, A. (2018). Comparing two partitions of non-equal sets of units. Advances in Methodology and Statistics 1(15), 1-23.
}
\author{
Marjan Cugmas
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (U = U, V = V, k = 1000)
{
    Empirical <- MRI(U = U, V = V)
    indexes <- NULL
    for (i in 1:k) {
        indexes[i] <- MRI(U = sample(U), V = sample(V))
    }
    (Empirical - mean(indexes))/(1 - mean(indexes))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
