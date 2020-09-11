\name{correctDist}
\alias{correctDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Corrected Euclidian Distance
}
\description{
It computes corrected Euclidian distances among the units.
}
\usage{
correctDist(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The data frame with units by rows and variables by columns. When the network data are analysed, the \code{data} is an adjacency matrix.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns the object of type "distance".
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
correctDist <- function(data){
  razdalje <- matrix(nrow = dim(data)[1], ncol = dim(data)[2])
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      razdalje[i, j] <-
        sqrt(
          ((data[i,i] - data[j,j])**2) +
            ((data[i,j] - data[j,i])**2) +
            sum(((data[,i] - data[,j])**2)[c(-i,-j)] +
                  ((data[i,] - data[j,])**2)[c(-i,-j)]))
    }
  }
  return(as.dist(razdalje))
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
