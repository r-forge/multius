#' University of Ljubljana color palettes
#'
#' This function provides color palettes adhering to the official visual identity
#' of the University of Ljubljana. It includes the main palette, primary and secondary color schemes,
#' individual color gradations, and multiple divergent color palettes.
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
#' @param ... Additional arguments to be passed to the \code{colorRampPalette} function.
#'
#' @return A function that takes an integer argument n and returns a character vector of n colors.
#'
#' @examples
#' \dontrun{
#' pal <- ul_pal("primary")
#' pal(5)  # Generates a palette of 5 colors based on the "primary" palette
#'
#' pal <- ul_pal("divergent1", reverse = TRUE)
#' pal(7)  # Generates a reversed divergent palette of 7 colors
#' }
#' @importFrom grDevices colorRamp colorRampPalette
#' @export
ul_pal <- function(palette = "main", reverse = FALSE, ...) {
  ul_palettes <- list(
    `main` = c("#0033a0", "#0082C0", "#00B1AC", "#00694E",
               "#EACE12", "#CB511C", "#9A2F31", "#C43788"),

    `secondaryCold` = c("#0033a0", "#0082C0", "#00B1AC", "#00694E"),
    `secondaryWarm` = c("#EACE12", "#CB511C", "#9A2F31", "#C43788"),

    `red`     = c("#F9D6D4", "#E03127"),
    `antracit` = c("#DEDEDE", "#58595b"),
    `medium`     = c("#EDEEEE", "#A7A8AA"),

    `darkblue`   = c("#CCD6EC","#0033a0"),
    `navyblue`   = c("#CCE6F2", "#0082C0"),
    `turquoise`   = c("#CCEFEE", "#00B1AC"),
    `green`   = c("#CCE1DC", "#00694E"),

    `yellow`   = c("#FBF5D0", "#EACE12"),
    `orange`   = c("#F5DCD2", "#CB511C"),
    `burgundy`   = c("#EBD5D6", "#9A2F31"),
    `pink`   = c("#F3D7E7", "#C43788"),

    `redW`     = c("white", "#E03127"),
    `antracitW` = c("white", "#58595b"),
    `mediumW`     = c("white", "#A7A8AA"),
    `darkblueW`   = c("white", "#0033a0"),
    `navyblueW`   = c("white", "#0082C0"),
    `turquoiseW`   = c("white", "#00B1AC"),
    `greenW`   = c("white","#00694E"),
    `yellow`   = c("white","#EACE12"),
    `orangeW`   = c("white", "#CB511C"),
    `burgundyW`   = c("white", "#9A2F31"),
    `pinkW`   = c("white", "#C43788"),

    `divergent1`   = c("#9A2F31", "white", "#0082C0"),
    `divergent2`   = c("#9A2F31", "white", "#00694E"),
    `divergent3`   = c("#9A2F31", "white", "#0033A0"),
    `divergent4`   = c("#8A0F12", "white", "#103689")
  )

  pal <- ul_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}
