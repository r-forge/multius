#' University of Ljubljana colors
#'
#' This function provides a set of color schemes adhering to the official visual identity
#' of the University of Ljubljana. If no arguments are supplied, it returns all
#' available colors. If specific color names are supplied, it will return only
#' those colors.
#'
#' @param ... Optional character vectors. If color names are supplied, only those colors
#' will be returned. If no arguments are supplied, all available colors are returned. Avaiable colors are
#'red, antracit, medium, lajt, darkblue, navyblue, turquoise, green, yellow, orange, burgundy, and pink.
#'
#' @return A character vector of hexadecimal color codes.
#' If no arguments are supplied, all color codes are returned.
#' If specific color names are supplied, only the color codes for those colors are returned.
#'
#' @examples
#' ul_color("red", "green") # Returns hexadecimal color codes for red and green
#' ul_color() # Returns all available colors
#'
#' @export
ul_color <- function(...) {
  ul_colors <- c(
    `red`     = "#E03127",
    `antracit` = "#58595b",
    `medium`     = "#A7A8AA",
    `lajt`    = "#E8E9EA",

    `darkblue`   = "#0033a0",
    `navyblue`   = "#0082C0",
    `turquoise`   = "#00B1AC",
    `green`   = "#00694E",

    `yellow`   = "#EACE12",
    `orange`   = "#CB511C",
    `burgundy`   = "#9A2F31",
    `pink`   = "#C43788"
    )

  colorNames <- names(ul_colors)
  ul_colors <- as.vector(ul_colors)
  attr(ul_colors, "colorNames") <- colorNames

  cols <- c(...)
  if (is.null(cols)) {
    return(ul_colors)
  } else {
      return(ul_colors[attributes(ul_colors)$colorNames %in% cols])
    }
}
