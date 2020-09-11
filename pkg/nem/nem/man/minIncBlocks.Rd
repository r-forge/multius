\name{minIncBlocks}
\alias{minIncBlocks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minimal number of inconsistent blocks
}
\description{
Calculate the number of inconsistent blocks between the empirical and ideal blockmodel and also provide the order of groups which is the closest to the ideal one.
}
\usage{
minIncBlocks(res, compIM, mustBeConnected = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{the object returned by the function \code{optRandPar} in package \code{blockmodeling}}
  \item{compIM}{the image matrix to be compared with, i.e., the ideal image matrix, in \code{matrix} form}
  \item{mustBeConnected}{wheter only one component must be present in the blockmodel or not}
}

\details{
It compare all possile permutations of rows and columns orders of the empirical and ideal image matrices. For each comparison it calculats the minimal number of inconsistent blocks. It returns the minimal number of inconsistent blocks.

Three possible block types can be specified in \code{compIM}: com, nul, any.

When \code{mustBeConnected} is set to \code{TRUE}, the function returns the value of \code{error} which is equal to the number of blocks that are not of type \code{any}.
}
\value{
It return the list:
  \item{error}{the number of inconsistent blocks}
  \item{order}{the order of clusters}
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
minIncBlocks <- function(res, compIM, mustBeConnected = FALSE){
  IM <- IM(res)[,,]
  k <- nrow(IM)
  all.perms <- permn(1:k)

  diags <- sapply(1:length(all.perms), function(x) {sum((IM[all.perms[[x]], all.perms[[x]]] != compIM) * ifelse(compIM=="any", yes = 0, no = 1))})

  if (mustBeConnected == TRUE) {
    n.comp <- components(graph_from_adjacency_matrix(ifelse(IM=="com", yes = 1, no = 0)))$no
    if (n.comp > 1) return(list("error" = sum(compIM != "any"), "order" = all.perms[[which.min(diags)]]))
    if (n.comp == 1) return(list("error" = min(diags), "order" = all.perms[[which.min(diags)]]))
  }
  if (mustBeConnected == FALSE) {
    return(list("error" = min(diags), "order" = all.perms[[which.min(diags)]]))
  }
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
