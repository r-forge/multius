\name{transition}
\alias{transition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating the network considering mechanisms
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
transition(initial, k = 50000, theta, max.friends, lambda, type.sim = "c", q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initial}{
initial network of class \code{matrix}
}
  \item{k}{
the number of iterations
}
  \item{theta}{
vector of coefficients (strenghts of mecahnisms)
}
  \item{max.friends}{
the max number of possible friends
}
  \item{lambda}{
the power on the vector of probabilities for creating or dissolving the links
}
  \item{type.sim}{
the type of simulations
}
  \item{q}{
the expected (desired) density of the network
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{initial}{generated network}
\item{statistike.mat}{the triad census after each iteration (in columns are the numbers of different types of triads; after each iteration in rows)}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (initial, k = 50000, theta, max.friends, lambda, type.sim = "c",
    q)
{
    statistike.mat <- matrix(NA, nrow = k, ncol = 15)
    for (i in 1:k) {
        actor <- sample(nrow(initial), size = 1)
        l.friends <- which(initial[actor, ] == 1)
        nonfriends <- which(initial[actor, ] == 0)
        if (type.sim == "a") {
            same.more <- same.popular.others.than.me(initial,
                actor, error = 5) == 1 | more.popular.others.than.me(initial,
                actor) == 1
            same.more.ids <- which(same.more)
            less.ids <- setdiff(x = 1:nrow(initial), y = same.more.ids)
            possible.friends <- intersect(nonfriends, same.more.ids)
            unpopular.friends <- intersect(l.friends, less.ids)
            probabilities <- calculate.probabilies(initial, theta = theta,
                actor)
            lenPosFrieds <- length(possible.friends)
            lenUnPopFriends <- length(unpopular.friends)
            if ((length(l.friends) <= max.friends) & ((lenPosFrieds +
                lenUnPopFriends) > 0)) {
                addFriend <- runif(1) < lenPosFrieds/(lenPosFrieds +
                  lenUnPopFriends)
                if (addFriend == TRUE) {
                  initial[actor, mySample(possible.friends, prob = probabilities[possible.friends]^lambda,
                    1)] <- 1
                }
                else {
                  initial[actor, mySample(unpopular.friends,
                    prob = (1 - probabilities[unpopular.friends])^lambda,
                    1)] <- 0
                }
            }
            if ((length(l.friends) > max.friends)) {
                initial[actor, mySample(l.friends, prob = ((1 -
                  probabilities[l.friends])^lambda), 1)] <- 0
            }
        }
        if (type.sim == "b") {
            probabilities <- calculate.probabilies(initial, theta = theta,
                actor)
            if ((length(l.friends) < nrow(initial))) {
                initial[actor, mySample(nonfriends, prob = ((probabilities[nonfriends])^lambda),
                  1)] <- 1
            }
            if ((length(l.friends) > max.friends)) {
                initial[actor, mySample(l.friends, prob = ((1 -
                  probabilities[l.friends])^lambda), 1)] <- 0
            }
        }
        if (type.sim == "c") {
            probabilities <- calculate.probabilies(initial, theta = theta,
                actor)
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
            statistike.mat[i, ] <- summary(initial ~ triadcensus)
        }
    }
    return(list(initial, statistike.mat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
