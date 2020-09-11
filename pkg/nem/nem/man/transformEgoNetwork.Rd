\name{transformEgoNetwork}
\alias{transformEgoNetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform Ego Network to Complete Network
}
\description{
The function transform ego network to complete network.
}
\usage{
transformEgoNetwork(egocentric_network, symmetrize = FALSE, binarize = TRUE, binarize.type = "prob")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{egocentric_network}{A \code{matrix} or \code{data.frame} with at least two columns. The first column contains the IDs of the egos (which can appear in one or several rows). The other columns contains the IDs of the alters.}
    \item{symmetrize}{Indicate whether an asymmetric network has to be symmetrized.}
    \item{binarize}{Indicate whether a valued network has to be binarized.}
    \item{binarize.type}{If \code{prob} a given link is keeped if the weigh is higher as it would be expected by chance (see code for more details or Schaefer et al. article), if \code{simple} all links are keept.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns the network of class \code{matrix} with rownames and columnames corresponding to the ego's IDs.
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
