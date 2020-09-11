\name{randomize.valued.network}
\alias{randomize.valued.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating randomized valued networks
}
\description{
It randomizes a given symmetric valued network. Different types of randomisations are possible.
}
\usage{
randomize.valued.network(network = network, type = "weights.preserving")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{a symmetric valued network of type \code{matrix}}
  \item{type}{type of randomisation (\code{weights.preserving}, \code{degree.preserving}, \code{density.preserving})}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a randomized valued network of class \code{matrix}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
