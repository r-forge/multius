#' @title Marjan Color Palette
#'
#' @description This function returns a color palette from the Marjan color schemes. The Marjan color schemes are a set of predefined color palettes.
#'
#' @param palette A string representing the color palette to use. Available palettes are \code{"main"}, \code{"myBlue"}, \code{"myRed"}, \code{"myOrange"}, \code{"myGreen"}, \code{"myJungle"}, \code{"divergentPolar"}, \code{"divergentNeutral"}, \code{"divergentNeutral2"}. Default is \code{"main"}.
#' @param reverse Logical. If TRUE, the order of the colors in the palette is reversed. Default is FALSE.
#' @param ... Additional arguments passed to the colorRampPalette function.
#'
#' @return A colorRampPalette function representing the chosen Marjan color palette.
#'
#' @examples
#' \dontrun{
#' pal <- marjan_pal("myBlue")
#' colors <- pal(10)  # Generate 10 colors from the "myBlue" Marjan color palette
#'
#' library(ggplot)
#' ggplot(mtcars, aes(x = gear)) +
#' geom_bar(fill = marjan_color("myBlue"))
#'
#' }
#'
#' @seealso \code{\link[grDevices]{colorRampPalette}}
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
marjan_pal <- function(palette = "main", reverse = FALSE, ...) {
  marjan_palettes <- list(
    "main" = c("#44546A", "#9CBE53", "#BC2F2B", "#F19A00", "#06A484"),

    "myBlue" = c("#44546A", "#B4BBC3"),
    "myRed" = c("#BC2F2B", "#E4ACAA"),
    "myOrange" = c("#F19A00", "#F9D799"),
    "myGreen" = c("#9CBE53", "#D7E5BA"),
    "myJungle" = c("#06A484", "#9BDBCE"),

    'divergentPolar' = c("#BC2F2B", "white", "#06A484"),
    'divergentNeutral' = c("#BC2F2B", "white", "#44546A"),
    'divergentNeutral2' = c("#000D5E", "white", "#5E0000")
  )

  pal <- marjan_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}
