% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_fill_marjan.R
\name{scale_fill_marjan}
\alias{scale_fill_marjan}
\title{Marjan color scale for fill aesthetic}
\usage{
scale_fill_marjan(palette = "main", discrete = TRUE, reverse = FALSE, ...)
}
\arguments{
\item{palette}{A string representing the color palette to use. Available palettes are \code{"main"}, \code{"myBlue"}, \code{"myRed"}, \code{"myOrange"}, \code{"myGreen"},
\code{"myJungle"}, \code{"divergentPolar"}, \code{"divergentNeutral"}, \code{"divergentNeutral2"}. Default is \code{"main"}.}

\item{discrete}{Logical. If \code{TRUE} (default), a discrete color scale is used. If \code{FALSE}, a continuous color gradient is used.}

\item{reverse}{Logical. If \code{TRUE}, the order of the colors in the palette is reversed. Default is \code{FALSE}.}

\item{...}{Additional arguments passed on to the underlying scale function (`discrete_scale` or `scale_fill_gradientn`).}
}
\value{
A ggplot2 scale that can be added to a ggplot object.
}
\description{
This function provides a color scale for the fill aesthetic using the Marjan color palette. The Marjan palette is a customized color palette that provides a range of distinct and attractive colors.
}
\examples{
\dontrun{
library(ggplot2)
ggplot(mtcars, aes(x = gear, fill = as.factor(gear))) +
 geom_bar(position = "dodge") +
 scale_fill_marjan("main")
}

}
\seealso{
\code{\link[ggplot2]{scale_fill_discrete}}, \code{\link[ggplot2]{scale_fill_gradientn}}
}
