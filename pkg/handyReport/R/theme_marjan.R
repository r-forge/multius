#' Marjan's ggplot2 theme
#'
#' This function creates a custom ggplot2 theme, with various formatting
#' adjustments. It sets the panel grid minor and axis ticks to blank, modifies text
#' elements (including strip text, legend, axis titles, plot title, subtitle, and caption),
#' and ensures that Google font "Lato" is used where specified. The function also allows
#' to change the position of the legend.
#'
#' @param legend.position Character specifying the position of the legend on the plot.
#' The options are \code{"top"}, \code{"bottom"}, \code{"left"}, \code{"right"}, \code{"none"}. Default is \code{"top"}.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = disp)) +
#'  geom_point() +
#'  labs(title = "Title",
#'  subtitle = "Subtitle",
#'  caption = "Caption",
#'  y = "Y axis title",
#'  x = "X axis title") +
#'  theme_marjan(legend.position = "bottom")
#' print(p)
#' }
#'
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_files font_add_google
#' @importFrom ggplot2 theme_gray element_blank element_text theme
#' @export
theme_marjan <- function(legend.position = "top"){
  showtext_auto()
  if (!is.element(el = TRUE, set = font_files()$family %in% "Lato")) {
    try({font_add_google("Lato")}, silent = TRUE)
    showtext_auto()
  }

  theme_gray()
  theme(
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),

    #text elements
    strip.text.x = element_text(colour = 'black'),
    legend.position =  legend.position,
    legend.title = element_text(face = "bold", family="Lato"),
    axis.title = element_text(face="bold", family="Lato"),
    plot.title = element_text(face="bold", family="Lato", size = 14, hjust = 0, margin = margin(0,0,8,0)),
    plot.subtitle = element_text(family="Lato", hjust = 0, margin = margin(0,0,8,0)),
    plot.caption = element_text(family="Lato", hjust = 1, margin = margin(12,0,0,0))
  )
}
