\name{numberOfClusters}
\alias{numberOfClusters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The best number of clusters
}
\description{

}
\usage{
numberOfClusters(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The vector of decreasing values.}
  \item{w}{The weight (how big has to be the \eqn{i}-th difference compared to \eqn{i+1}-th, so the next will be keept).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A singe value - the most appropriate number of clusters.
}
\references{
%% ~put references to the literature/web site here ~
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
numberOfClusters <- function(x, w = 0.4){
  razl <- diff(x)
  ff <- NULL
  for (i in 1:(length(razl)-1)){ff[i] <- razl[i+1] <= razl[i]*w}
  return(which.max(which(ff==TRUE)))
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
