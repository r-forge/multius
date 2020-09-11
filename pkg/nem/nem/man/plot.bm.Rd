\name{plot.bm}
\alias{plot.bm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot blockmodel as a graph
}
\description{
The function plot the blockmodel as a graph.
}
\usage{
plot.bm(res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{the object returned by \code{oprRandParC} from \code{blockmodeling} package}
  \item{clu}{the object returned by \code{clu} from \code{blockmodeling} package or simmilar}
  \item{im}{the object returned by \code{IM} from \code{blockmodeling} package or simmilar}
  \item{useRez}{wheter to use \code{res} instead of \code{clu} and \code{im}}
  \item{arrowhead.cex}{the size of head of an arrow}
  \item{loop.cex}{the size of the loop}
  \item{...}{see \code{?plot.network} in \code{network} package}
}
\details{
The heart of the function is another function \code{plot.network} from \code{network} package. You have to specifiy only \code{res} or only \code{clu} and \code{im}.
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
Marjan Cugmas
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{optRandParC}, \code{plot.network}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
