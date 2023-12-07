#' @title University of Ljubljana theme (fill) scale
#' @description Applies color fill to the given data based on the University of Ljubljana's visual identity guidelines.
#' @param palette A character string indicating the color palette to use. Default is \code{"primary"}. See \code{?ul_pal} for other options.
#' @param discrete A logical indicating whether the scale is discrete. Default is \code{TRUE}.
#' @param reverse A logical indicating whether to reverse the order of colors in the palette. Default is \code{FALSE}.
#' @param midpoint A numeric value indicating the midpoint for the color scale in case of a divergent color scale (default is \code{NA}).
#' @param neutralColor A character string specifying the color to use for neutral values (default is \code{"white"}).
#' @param guide A guide function specification, such as \code{guide_colorbar()} or \code{guide_legend()}, used to modify specific guide properties (default is \code{guide_colourbar(frame.colour = "#58595b", title.vjust = 0.8)}).
#' @param ... Other arguments passed on to the scale functions (\code{discrete_scale}, \code{scale_fill_gradientn} or \code{scale_fill_gradient2}).
#' @return A discrete or gradient fill scale.
#' @details
#' This function returns a color fill scale to be used with ggplot2. It uses color palettes defined according to the visual identity of the University of Ljubljana.
#' @seealso \code{\link[ggplot2]{scale_fill_gradientn}}, \code{\link[ggplot2]{scale_fill_gradient2}}, \code{\link[ggplot2]{discrete_scale}}
#' @examples
#' ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
#' geom_density(alpha = 0.7) +
#' scale_fill_ul(discrete = TRUE)
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn scale_fill_gradient2 guide_colourbar
#' @export
scale_fill_ul <- function(palette = "primary", discrete = TRUE, reverse = FALSE, midpoint = NA, neutralColor = "white", guide = guide_colourbar(frame.colour = "#58595b", title.vjust = 0.8), ...) {
  if (discrete) {
    pal <- ul_pal(palette = palette, reverse = reverse, neutralColor = neutralColor)
    return(discrete_scale("fill", paste0("ul_", palette), palette = pal, ...))
  }
  if (!discrete){
    if (palette %in% attr(ul_color(), "colorNames")) palette <- paste0(palette, "W")
    pal <- ul_pal(palette = palette, reverse = reverse, neutralColor = neutralColor)
    if (is.na(midpoint)) {return(scale_fill_gradientn(colours = pal(256), guide = guide, ...))}
    if (!is.na(midpoint)) {barve <- pal(3); return(scale_fill_gradient2(low = barve[1], mid = barve[2], high = barve[3], midpoint = midpoint, guide = guide,...))}
  }
}
