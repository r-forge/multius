#' Fill scale for University of Ljubljana color palettes
#'
#' This function provides fill scales using the official visual identity
#' color palettes of the University of Ljubljana for use in ggplot2.
#'
#' @param palette Character. The name of the palette to use.
#' - Main: \code{"main"} (default)
#' - Secondary (cold): \code{"secondaryCold"}
#' - Secondary (warm): \code{"secondaryWarm"}
#' - Individual colors: \code{"red"}, \code{"antracit"}, \code{"medium"}, \code{"lajt"}, \code{"darkblue"},
#' \code{"navyblue"}, \code{"turquoise"}, \code{"green"}, \code{"yellow"}, \code{"orange"}, \code{"burgundy"},
#' \code{"pink"}
#' - Divergent: \code{"divergent1"}, \code{"divergent2"}, \code{"divergent3"}, \code{"divergent4"}
#' @param reverse Logical. If \code{TRUE}, reverses the order of colors in the palette. Default is \code{FALSE}.
#' @param discrete Logical. If \code{TRUE} (default), a discrete color scale is used. If \code{FALSE}, a continuous color gradient is used.
#' @param midpoint The midpoint (in data value) of the diverging scale.
#' @param ... Additional arguments to be passed to the discrete_scale or scale_fill_gradientn functions.
#'
#' @return A fill scale that can be added to a ggplot2 plot.
#' @seealso \code{\link[ggplot2]{scale_fill_discrete}}, \code{\link[ggplot2]{scale_fill_gradientn}}
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot(aes(fill = class)) +
#'   scale_fill_ul("primary", discrete = TRUE)
#' }
#'
#' @export
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn scale_fill_gradient2
scale_fill_ul <- function(palette = "main", discrete = TRUE, reverse = FALSE, midpoint = NA, ...) {
  if (discrete) {
    pal <- ul_pal(palette = palette, reverse = reverse)
    return(discrete_scale("fill", paste0("ul_", palette), palette = pal, ...))
  }
  if (!discrete){
    if (palette %in% c("red", "antracit", "medium", "darkblue", "navyblue", "turquoise","green", "yellow",  "orange",  "burgundy",  "pink")) palette <- paste0(palette, "W")
    pal <- ul_pal(palette = palette, reverse = reverse)
    if (is.na(midpoint)) {rm(midpoint); return(scale_fill_gradientn(colours = pal(256), ...))}
    if (!is.na(midpoint)) {barve <- pal(3); return(scale_fill_gradient2(low = barve[1], mid = barve[2], high = barve[3], midpoint = midpoint, ...))}
  }
}
