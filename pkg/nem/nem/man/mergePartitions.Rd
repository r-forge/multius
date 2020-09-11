\name{mergePartitions}
\alias{mergePartitions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merging Partitions
}
\description{
This function creates a data frame with the infomration to which cluster a given unit belonged in the first and in the second observation (time period). The incomers and outgoings are marked.
}
\usage{
mergePartitions(podatki_ena, podatki_dva)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{podatki_ena}{The data frame with two columns (the data are for the units observed in the first time period): there are the IDs of the units in the first column and there is an infomration to which cluster a given unit belongs in the second column.}
  \item{podatki_dva}{The data frame with two columns (the data are for the units observed in the second time period): there are the IDs of the units in the first column and there is an infomration to which cluster a given unit belongs in the second column.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a data frame with three columns. There are the IDs of the units in the first column, following by the information to which cluster a given unit belonged in the first (second column) and the second (third column) time period. The incomers and outgoings are indicated if present.
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
function (podatki_ena, podatki_dva)
{
    osip <- "incoming"
    novi <- "outgoing"
    rownames(podatki_ena) <- podatki_ena[, 1]
    rownames(podatki_dva) <- podatki_dva[, 1]
    skupni_podatki <- merge(podatki_ena, podatki_dva, by = "row.names",
        all = TRUE)[, c(1, 3, 5)]
    skupni_podatki <- data.frame(skupni_podatki)
    skupni_podatki[, 2] <- as.character(skupni_podatki[, 2])
    skupni_podatki[, c(2)][is.na(skupni_podatki[, c(2)])] <- osip
    skupni_podatki[, 3] <- as.character(skupni_podatki[, 3])
    skupni_podatki[, c(3)][is.na(skupni_podatki[, c(3)])] <- novi
    return(skupni_podatki)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
