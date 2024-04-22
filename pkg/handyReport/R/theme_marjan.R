#' Marjan's ggplot2 theme
#'
#' This function creates a custom ggplot2 theme, with various formatting
#' adjustments. It sets the panel grid minor and axis ticks to blank, modifies text
#' elements (including strip text, legend, axis titles, plot title, subtitle, and caption).
#' The function also allows to change the position of the legend.
#'
#' @param legend.position Character specifying the position of the legend on the plot.
#' The options are \code{"top"}, \code{"bottom"}, \code{"left"}, \code{"right"}, \code{"none"}. Default is \code{"top"}.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'ggplot(mtcars, aes(x = mpg, y = disp, color = as.factor(am))) +
#'  geom_point() +
#'  labs(title = "Title",
#'       color  = "Legend title",
#'      subtitle = "Subtitle",
#'      caption = "Caption",
#'       y = "Y axis title",
#'       x = "X axis title") +
#'  theme_marjan(legend.position = "bottom")
#' }
#'
#' @importFrom ggplot2 theme_gray element_blank element_text theme
#' @export
theme_marjan <- function(legend.position = "top"){
  theme_gray()
  theme(
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),

    #text elements
    strip.text.x = element_text(colour = 'black'),
    legend.position =  legend.position,
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face="bold"),
    plot.title = element_text(face="bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(hjust = 1)
  )
}

