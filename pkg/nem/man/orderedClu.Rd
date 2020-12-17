\name{orderdClu}
\alias{orderdClu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ordered clusters in a matrix network representation
}
\description{
This function reorder the groups in an image matrix in such a way that the blocks with a higher average weigh are placed on the left hand side of the matrix.
}
\usage{
orderdClu(res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{The object produced by the function \code{oprRandParC} from \code{blockmodeling} package.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{tclu}{The vector indication to which group a certain unit belongs.}
\item{IMg}{Reordered image matrix.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marjan Cugmas, Aleš Žiberna
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[myPhD]{orderedIM}}
}
\examples{
orderdClu <- function(res){
  means<-fun.by.blocks(res, ignore.diag=TRUE)
  crit<-colSums(means)+rowSums(means)
  r <- order(crit, decreasing = TRUE)
  a <- IM(res)[1,,]
  a <- a[as.vector(r), as.vector(r)]
  IMg <- array(NA, dim = c(1, max(r), max(r)))
  (IMg[1,,] <-  a)
  r<-rank(-crit)
  names(r) <- 1:max(clu(res))
  tclu <- r[as.character(clu(res))]
  return(list(tclu, IMg))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
