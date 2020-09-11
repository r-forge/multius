\name{simulate.RL}
\alias{simulate.RL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Relocating Links algorithm
}
\description{
The function generate random network considering the selected types of triads.
}
\usage{
simulate.RL(ideal.net, random.net, terms = "forb", k = 100, custom.terms = NULL, cr.type = "sum")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ideal.net}{
ideal network structure of class \code{matrix}
}
  \item{random.net}{
random or initial network of class \code{matrix}
}
  \item{terms}{
which types of triads has to be considered (allowed \code{allow}, forbidden \code{forb}, all \code{all} or custom \code{cust})
}
  \item{k}{
the number of iterations
}
  \item{custom.terms}{
additional terms to be considered (if \code{terms = "cust"})
}
  \item{cr.type}{
how to calculate CR value (allways use \code{sum})
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{new.network}{generated network}
\item{change.ratio.vec}{vector of CR values, each value is calculated after each iteration}
%% ...
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
function (ideal.net, random.net, terms = "forb", k = 100, custom.terms = NULL,
    cr.type = "sum")
{
    teoreticna.porazdelitev <- summary(ideal.net ~ triadcensus)
    if (terms == "forb")
        allowed.terms <- names(teoreticna.porazdelitev)[teoreticna.porazdelitev ==
            0]
    if (terms == "allow")
        allowed.terms <- names(teoreticna.porazdelitev)[teoreticna.porazdelitev !=
            0]
    if (terms == "all")
        allowed.terms <- names(teoreticna.porazdelitev)
    if (terms == "cust")
        allowed.terms <- names(teoreticna.porazdelitev)[custom.terms]
    change.ratio.vec <- NULL
    new.network <- random.net
    for (i in 1:k) {
        pov <- sample(which(random.net == 0)[is.element(which(random.net ==
            0), which(diag(1, nrow = n) == 1)) == F], size = 1)
        npov <- sample(which(random.net == 1), size = 1)
        new.network[pov] <- 1
        new.network[npov] <- 0
        s.rand <- summary(random.net ~ triadcensus)
        s.new <- summary(new.network ~ triadcensus)
        if (cr.type == "mean") {
            nova.teoreticna <- mean((s.new[allowed.terms] - teoreticna.porazdelitev[allowed.terms]))
            stara.teoreticna <- mean((s.rand[allowed.terms] -
                teoreticna.porazdelitev[allowed.terms]))
        }
        if (cr.type == "sum") {
            nova.teoreticna <- sum((s.new[allowed.terms] - teoreticna.porazdelitev[allowed.terms])^2)
            stara.teoreticna <- sum((s.rand[allowed.terms] -
                teoreticna.porazdelitev[allowed.terms])^2)
        }
        change.ratio.vec[i] <- change.ratio <- nova.teoreticna/stara.teoreticna
        if (is.nan(change.ratio) == FALSE) {
            if (change.ratio < 1)
                random.net <- new.network
            else new.network <- random.net
        }
        else new.network <- random.net
    }
    return(list(new.network, change.ratio.vec))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
