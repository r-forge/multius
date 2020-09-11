\name{critFunCMC}
\alias{critFunCMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the value of a criterion function
}
\description{
This is a modification of the function \code{critFunCMC} from package \code{blockmodeling}. It calculate the value of a criterion function by considering the ideal block types.
}
\usage{
critFunCMC(res, compIM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{an object returned by a function \code{oprRandParC} from \code{blockmodeling} package}
  \item{compIM}{an ideal blockmodel with possible blocks of type \code{any}}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It return a single value.
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
\code{\link{minIncBlocks}}
}
\examples{
critFunCMC <- function(res, compIM){
  ERR <- critFunC(M = res$M,
                  approaches = res$initial.param$dots$approach,
                  clu = clu(res),
                  blocks = res$initial.param$dots$blocks,
                  blockTypeWeights = res$initial.param$dots$blockTypeWeights)

  im.ord <- minIncBlocks(res = res, compIM = compIM)$order
  return(sum(ERR$EM[,im.ord,im.ord] * ifelse(compIM=="any", yes = 0, no = 1)))
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
