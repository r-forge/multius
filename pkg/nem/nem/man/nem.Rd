\name{nem}
\alias{nem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating the networks by considering the selected mechanisms
}
\description{
This function can be used to generate the networks by considering different mechanisms.
}
\usage{
nem(initial, formula, theta, k = 5000, q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initial}{The initial network of class \code{matrix} (it can be empty).
}
  \item{formula}{The \code{list} of terms (each term is a function -- see nem-temrs).}
  \item{theta}{The vector of parameters (mechanisms' strenghts).}
  \item{k}{The number of iterations.}
  \item{q}{The probability with which the link is established at each iteration.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns the generated network of class \code{matrix}.
}
\references{
Cugmas, M., Ferligoj, A., & Ziberna, A. (2018). Generating global network structures by triad types. PloS one, 13(5), e0197514.
}
\author{
Marjan Cugmas
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
function (initial, formula, theta, k = 5000, q)
{
    for (i in 1:k) {
        actor <- sample(nrow(initial), size = 1)
        l.friends <- which(initial[actor, ] == 1)
        nonfriends <- which(initial[actor, ] == 0)
        X <- matrix(NA, nrow = nrow(initial), ncol = length(formula))
        for (j in 1:length(formula)) {
            X[, j] <- formula[[j]](initial, actor = actor)
        }
        X[is.nan(X)] <- 0
        probabilities <- X \%*\% theta
        if (sample(c(0, 1), prob = c(1 - q, q), 1) == 1) {
            if (length(nonfriends) > 0) {
                initial[actor, sample(which(probabilities >=
                  quantile(probabilities)[4]), 1)] <- 1
            }
        }
        if (sample(c(0, 1), prob = c(1 - q, q), 1) == 0) {
            if (length(l.friends) > 0) {
                initial[actor, sample(which(probabilities <=
                  quantile(probabilities)[2]), 1)] <- 0
            }
        }
    }
    diag(initial) <- 0
    return(initial)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
