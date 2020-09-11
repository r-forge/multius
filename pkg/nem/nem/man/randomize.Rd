\name{randomize}
\alias{randomize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Randomize network
}
\description{
Return randomized binary network with the same density as the initial network. Loops are not assumed. The randomized network is asymmetric.
}
\usage{
randomize(network)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{the binary network, of class \code{matrix}, that has to be randomized}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
randomize <- function(network){
  random <- matrix(sample(network), nrow = nrow(network))
  ndiag <- sum(diag(random))
  diag(random) <- -1

  random[sample(which(random == 0), replace = F, size = ndiag)] <- 1
  diag(random) <- 0
  return(random)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
