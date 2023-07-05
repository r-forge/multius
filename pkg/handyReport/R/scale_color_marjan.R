#' @title Marjan color scale for color aesthetic
#'
#' @description This function provides a color scale for the color aesthetic using the Marjan color palette. The Marjan palette is a customized color palette that provides a range of distinct and attractive colors.
#'
#' @param palette A string representing the color palette to use. Available palettes are \code{"main"}, \code{"myBlue"}, \code{"myRed"}, \code{"myOrange"}, \code{"myGreen"},
#' \code{"myJungle"}, \code{"divergentPolar"}, \code{"divergentNeutral"}, \code{"divergentNeutral2"}. Default is \code{"main"}.
#' @param discrete Logical. If \code{TRUE} (default), a discrete color scale is used. If \code{FALSE}, a continuous color gradient is used.
#' @param reverse Logical. If \code{TRUE}, the order of the colors in the palette is reversed. Default is \code{FALSE}.
#' @param ... Additional arguments passed on to the underlying scale function (`discrete_scale` or `scale_color_gradientn`).
#'
#' @return A ggplot2 scale that can be added to a ggplot object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'ggplot(mtcars, aes(x = mpg, y = disp, size = hp, color = carb)) +
#' geom_point() +
#' scale_color_marjan(palette = "divergentPolar", discrete = FALSE)
#' }
#'
#' @seealso \code{\link[ggplot2]{scale_color_discrete}}, \code{\link[ggplot2]{scale_color_gradientn}}
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#'
#' @export
scale_color_marjan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- marjan_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


