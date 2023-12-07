#' University of Ljubljana color palettes
#'
#' This function provides color palettes adhering to the official visual identity
#' of the University of Ljubljana. It includes the primary and secondary palettes,
#' individual color gradations, and multiple divergent color palettes.
#'
#' @param palette Character. The name of the palette to use.
#'
#' \itemize{
#'   \item Primary: \code{"primary"} (default)
#'   \item Secondary (cold): \code{"secondaryCold"}
#'   \item Secondary (warm): \code{"secondaryWarm"}
#'   \item Individual colors: \code{"red"}, \code{"antracit"}, \code{"medium"}, \code{"lajt"}, \code{"darkblue"}, \code{"navyblue"}, \code{"turquoise"}, \code{"green"}, \code{"yellow"}, \code{"orange"}, \code{"burgundy"}, \code{"pink"}
#'   \item Divergent: \code{"redBlue"}, \code{"redGreen"}, \code{"blueTurquise"}, \code{"blueYellow"}
#' }
#' @param reverse Logical. If \code{TRUE}, reverses the order of colors in the palette. Default is \code{FALSE}.
#' @param neutralColor Define the color used as neutral (default \code{"white"}).
#' @param ... Additional arguments to be passed to the \code{colorRampPalette} function.
#'
#' @return A function that takes an integer argument n and returns a character vector of n colors.
#'
#' @examples
#' pal <- ul_pal("redBlue", reverse = TRUE)
#' pal(7)  # Generates a reversed divergent palette of 7 colors
#' @importFrom grDevices colorRamp colorRampPalette
#' @export
ul_pal <- function(palette = "primary", neutralColor = "white", reverse = FALSE, ...) {
  neutralColor <- ifelse(neutralColor %in% attr(ul_color(), "colorNames"), yes = ul_color(neutralColor), no = neutralColor)

  ul_palettes <- list(
    `primary` = c("#0033a0", "#0082C0", "#00B1AC", "#00694E",
               "#EACE12", "#CB511C", "#9A2F31", "#C43788"),

    `cold` = c("#0033a0", "#0082C0", "#00B1AC", "#00694E"),
    `warm` = c("#EACE12", "#CB511C", "#9A2F31", "#C43788"),

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

    `redW`     = c(neutralColor, "#E03127"),
    `antracitW` = c(neutralColor, "#58595b"),
    `mediumW`     = c(neutralColor, "#A7A8AA"),
    `darkblueW`   = c(neutralColor, "#0033a0"),
    `navyblueW`   = c(neutralColor, "#0082C0"),
    `turquoiseW`   = c(neutralColor, "#00B1AC"),
    `greenW`   = c(neutralColor,"#00694E"),
    `yellow`   = c(neutralColor,"#EACE12"),
    `orangeW`   = c(neutralColor, "#CB511C"),
    `burgundyW`   = c(neutralColor, "#9A2F31"),
    `pinkW`   = c(neutralColor, "#C43788"),

    `redBlue`   = c("#9A2F31", neutralColor, "#0082C0"),
    `redGreen`   = c("#9A2F31", neutralColor, "#00694E"),
    `blueTurquise`   = c("#0082C0", neutralColor, "#00B1AC"),
    `blueYellow`   = c("#0082C0", neutralColor, "#EACE12")
  )

  pal <- ul_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}
