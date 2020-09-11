\name{iss03}
\alias{iss03}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating growing networks by considering some local mechanisms
}
\description{
This function generates random (growing) networks. The network does not neccesary to be growing.
}
\usage{
iss03(initial.network,
      lambda,
      kappa,
      newbie,
      internal.mechanisms,
      external.mechanisms,
      external.covariates,
      theta.im,
      theta.em,
      computeTriads = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initial.network}{the initial network, it can be an empty network}
  \item{lambda}{maximum out-degree}
  \item{kappa}{the factor for which the minimum number of iterations should be increased}
  \item{newbie}{the number of new employees, it can be vector if they are employeed in several waves}
  \item{internal.mechanisms}{the mechanisms that are based on the network structure}
  \item{external mechanisms}{the mechanisms that consider the external covariates (for now only the age)}
  \item{theta.im}{the vector of parameters' strenghts for internal mechanisms}
  \item{theta.em}{the vector of parameters' strenghts for external mechanisms}
  \item{computeTruads}{logical: should the triad census be coomputed at each iteration? (default if FALSE)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a list:
\item{networks}{the list of generated networks at different iterations}
\item{triads}{the triadcensus, calculated at each iteration}
\item{employing}{at which iteraions the employees were employeed}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marjan Cugmas and Ales Ziberna
}
\note{
This function differ from \code{nem} in the way how the non-links are set. This function also consider newcomers. This function also consider the external mechanisms/covariates (for now, only age is implemented).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{nem}}, \code{\link{nem-terms}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
