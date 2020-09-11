\name{common.friends}
\alias{common.friends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Common friends mechanism
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
common.friends(network = network, actor, as.asymmetric = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{symmetrix or asymmetric network of class \code{matrix}}
  \item{actor}{which actor has to be taken as reference}
  \item{as.asymmetric}{binary, when set to \code{TRUE} only symmetric ties are considered, but when set to \code{FALSE}, both symmetric and asymmetric ties are considered}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The vector of lenght equal the number of the units in the network. The value of the i-th element is the number of common friends between the actor and i-th unit in the network. Note: the actor is considered as being friend with himself.
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
