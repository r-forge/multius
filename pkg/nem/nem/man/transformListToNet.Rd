\name{transformListToNet}
\alias{transformListToNet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform temporal ego network to adjacency matrix
}
\description{
It transform the object returned by function \code{wnet} (and other simmilar functions) to the adjacency matrix. The time period to be considered can be set.
}
\usage{
transformListoToNet(net.list, n, from = 1, to = nrow(net.list))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net.list}{matrix with three columns (actor, alter, action) and k rows (k equals the number of iterations in \code{nem} algorithm)}
  \item{n}{number of units}
  \item{from}{the begining of the selected time period}
  \item{to}{the end of the selected time period}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns the network in the form of adjacency matrix.
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
\code{\link[myPhD]{nem}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
