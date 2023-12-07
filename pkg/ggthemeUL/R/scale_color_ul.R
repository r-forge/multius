#' @title University of Ljubljana theme (color) scale
#' @description Applies color scale to the given data based on the University of Ljubljana's visual identity guidelines.
#' @param palette A character string indicating the color palette to use. Default is \code{"primary"}. See \code{?ul_pal} for other options.
#' @param discrete A logical indicating whether the scale is discrete. Default is \code{TRUE}.
#' @param reverse A logical indicating whether to reverse the order of colors in the palette. Default is \code{FALSE}.
#' @param midpoint A numeric value indicating the midpoint for the color scale in case of a divergent color scale (default is \code{NA}).
#' @param neutralColor A character string specifying the color to use for neutral values (default is \code{"white"}).
#' @param guide A guide function specification, such as \code{guide_colorbar()} or \code{guide_legend()}, used to modify specific guide properties (default is \code{guide_colourbar(frame.colour = "#58595b", title.vjust = 0.8)}).
#' @param ... Other arguments passed on to the scale functions (\code{discrete_scale}, \code{scale_color_gradientn} or \code{scale_color_gradient2}).
#' @return A discrete or gradient color scale.
#' @details
#' This function returns a color scale to be used with ggplot2. It uses color palettes defined according to the visual identity of the University of Ljubljana.
#' @seealso \code{\link[ggplot2]{scale_color_gradientn}}, \code{\link[ggplot2]{scale_color_gradient2}}, \code{\link[ggplot2]{discrete_scale}}
#' @examples
#' ggplot(mtcars, aes(x = mpg, y = wt, color = factor(gear))) +
#' geom_point(size = 4) +
#' scale_color_ul()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn scale_color_gradient2 guide_colourbar
#' @export
scale_color_ul <- function(palette = "primary", discrete = TRUE, reverse = FALSE, midpoint = NA, neutralColor = "white", guide = guide_colourbar(frame.colour = "#58595b", title.vjust = 0.8), ...) {
  if (discrete) {
    pal <- ul_pal(palette = palette, reverse = reverse, neutralColor = neutralColor)
    return(discrete_scale("colour", paste0("ul_", palette), palette = pal, ...))
  }
  if (!discrete){
    if (palette %in% attr(ul_color(), "colorNames")) palette <- paste0(palette, "W")
    pal <- ul_pal(palette = palette, reverse = reverse, neutralColor = neutralColor)
    if (is.na(midpoint)) {return(scale_color_gradientn(colours = pal(256), guide = guide, ...))}
    if (!is.na(midpoint)) {barve <- pal(3); return(scale_color_gradient2(low = barve[1], mid = barve[2], high = barve[3], midpoint = midpoint, guide = guide,...))}
  }
}
