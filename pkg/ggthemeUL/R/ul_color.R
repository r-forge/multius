#' UL color theme
#'
#' This function provides a set of color schemes adhering to the official visual identity
#' of the University of Ljubljana. The colors are categorized into primary, secondary cold,
#' and secondary warm color schemes. If no arguments are supplied, it returns all
#' available colors. If specific color names are supplied, it will return only
#' those colors.
#'
#' The primary color scheme consists of red, antracit, medium, and lajt.
#' The secondary cold color scheme includes darkblue, navyblue, turquoise, and green.
#' The secondary warm color scheme is defined by yellow, orange, burgundy, and pink.
#'
#' @param ... Optional character vectors. If color names are supplied, only those colors
#' will be returned. If no arguments are supplied, all available colors are returned.
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

  cols <- c(...)
  if (is.null(cols))
    return (ul_colors)
  ul_colors[cols]
}
