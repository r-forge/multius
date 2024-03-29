% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KNNimp.R
\name{KNNimp}
\alias{KNNimp}
\title{KNN-imputation method}
\usage{
KNNimp(data, k = 10, scale = TRUE, meth = "weighAvg", distData = NULL)
}
\arguments{
\item{data}{A data frame with the data set.}

\item{k}{The number of nearest neighbours to use (defaults to 10).}

\item{scale}{Boolean setting if the data should be scale before finding the nearest neighbours (defaults to TRUE).}

\item{meth}{String indicating the method used to calculate the value to fill in each NA. Available values are \code{median} or \code{weighAvg} (the default).}

\item{distData}{Optionally you may sepecify here a data frame containing the data set that should be used to find the neighbours. This is usefull when filling in NA values on a test set, where you should use only information from the training set. This defaults to \code{NULL}, which means that the neighbours will be searched in data.}
}
\value{
A dataframe with imputed values.
}
\description{
Function that fills in all NA values using the k-nearest-neighbours of each case with NA values.
By default it uses the values of the neighbours and obtains an weighted (by the distance to the case) average of
their values to fill in the unknows. If \code{meth='median'} it uses the median/most frequent value, instead.
}
\details{
This function uses the k-nearest neighbours to fill in the unknown (NA) values in a data set. For each case with any NA value it will search for its k most similar cases and use the values of these cases to fill in the unknowns.
If \code{meth='median'} the function will use either the median (in case of numeric variables) or the most frequent value (in case of factors), of the neighbours to fill in the NAs. If \code{meth='weighAvg'} the function will use a
weighted average of the values of the neighbours. The weights are given by \code{exp(-dist(k,x)} where \code{dist(k,x)} is the euclidean distance between the case with NAs (x) and the neighbour k.
}
\note{
This is a slightly modified function from package \code{DMwR} by Luis Torgo. The modification allows the units with missing values at almost all variables.
}
\examples{
mtcars$mpg[sample(1:nrow(mtcars), size = 5, replace = FALSE)] <- NA
KNNimp(data = mtcars)
}
\references{
Torgo, L. (2010) Data Mining using R: learning with case studies, CRC Press (ISBN: 9781439810187).
}
\seealso{
\code{seqKNNimp}
}
\author{
Luis Torgo
}
