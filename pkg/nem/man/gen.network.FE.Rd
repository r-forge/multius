\name{gen.network.FE}
\alias{gen.network.FE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate the network with a given blockmodel and with a given level of errors
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gen.network.FE(type = type, randomShare = 0.4, symmetric = FALSE, n = 42, size = rep(1/nrow(type), nrow(type)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{a \code{matrix} containg types of blocks \code{"null"} or \code{"com"}}
  \item{randomShare}{a level of errors (a single value between 0 (ideal network) and 1 (random network))}
  \item{symmetric}{should the network be symmetric \code{TRUE} or asymmetric \code{FALSE}}
  \item{n}{number of unnits in a network}
  \item{size}{specify the size of each cluster; specify the vector of sizes (the elements of the vector have sum to 1)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a binary network of class \code{matrix}.
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
function (type = type, randomShare = 0.4, symmetric = F, n = 42,
    size = c(1/3, 1/3, 1/3))
{
    initialNetwork <- gen.network(type = type, errors = 0, symmetric = symmetric,
        n = n, size = size)
    newNetwork <- initialNetwork
    kriterij <- (1 - (sum(initialNetwork == 1)/length(initialNetwork))) *
        randomShare
    if (randomShare != 0) {
        while (mean(newNetwork[which(initialNetwork == 1)] ==
            0) <= kriterij) {
            newNetwork[sample(which(newNetwork == 0), size = 1)] <- 1
            newNetwork[sample(which(newNetwork == 1), size = 1)] <- 0
        }
        nloops <- sum(diag(newNetwork) == 1)
        diag(newNetwork) <- 1
        changeToOne <- sample(which(newNetwork == 0), size = nloops,
            replace = F)
        newNetwork[changeToOne] <- 1
        diag(newNetwork) <- 0
        return(newNetwork)
    }
    if (randomShare == 0)
        return(initialNetwork)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
