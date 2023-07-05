#' @title Marjan color scale for fill aesthetic
#'
#' @description This function provides a color scale for the fill aesthetic using the Marjan color palette. The Marjan palette is a customized color palette that provides a range of distinct and attractive colors.
#'
#' @param palette A string representing the color palette to use. Available palettes are \code{"main"}, \code{"myBlue"}, \code{"myRed"}, \code{"myOrange"}, \code{"myGreen"},
#' \code{"myJungle"}, \code{"divergentPolar"}, \code{"divergentNeutral"}, \code{"divergentNeutral2"}. Default is \code{"main"}.
#' @param discrete Logical. If \code{TRUE} (default), a discrete color scale is used. If \code{FALSE}, a continuous color gradient is used.
#' @param reverse Logical. If \code{TRUE}, the order of the colors in the palette is reversed. Default is \code{FALSE}.
#' @param ... Additional arguments passed on to the underlying scale function (`discrete_scale` or `scale_fill_gradientn`).
#'
#' @return A ggplot2 scale that can be added to a ggplot object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = gear, fill = as.factor(gear))) +
#'  geom_bar(position = "dodge") +
#'  scale_fill_marjan("main")
#' }
#'
#' @seealso \code{\link[ggplot2]{scale_fill_discrete}}, \code{\link[ggplot2]{scale_fill_gradientn}}
#'
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#'
#' @export
scale_fill_marjan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- marjan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("marjan_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

