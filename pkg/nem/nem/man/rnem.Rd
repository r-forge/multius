\name{rnem}
\alias{rnem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Network evolution model (list format)
}
\description{
It generates the networks, regarding the considered mechanisms and their strenghts.
}
\usage{
rnem(initial, formula, theta, k = 5000, q, b = 0.25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initial}{The initial network of class \code{matrix} (it can be empty).
}
  \item{formula}{The \code{list} of terms (each term is a function -- see nem-temrs).
}
  \item{theta}{The vector of parameters (mechanisms' strenghts).
}
  \item{k}{The number of iterations.}
  \item{q}{The probability with which the link is established at each iteration.}
  \item{b}{Share of units to be considered for creating or dissolving links.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a \code{list} with the following elements:
 \item{final}{A final binary network of class \code{matrix}.}
 \item{initial}{An initial network of class \code{matrix}.}
 \item{negative}{A \code{matrix} with three columns (alter ID, ego ID, action) and the number of rows equal to k. Action can only by 1 (created positive nomination) or 0 (no action taken).}
 \item{positive}{A \code{matrix} with three columns (alter ID, ego ID, action) and the number of rows equal to k. Action can only be -1 (created negative nomination) or 0 (no action taken).}
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
\code{\link[myPhD]{nem}}, \code{\link[myPhD]{transformListToNet}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
