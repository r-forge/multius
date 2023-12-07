#' Theme UL
#'
#' @description This function creates a ggplot2 theme that follows the visual identity of the University of Ljubljana.
#'
#' @param legend.position A character string that specifies the position of the legend. Default is \code{"top"}.
#' @param panel.background.fill A character string that specifies the HEX code for the color of the panel background. Default is \code{"#E8E9EA"}.
#' @param plot.background.fill A character string that specifies the HEX code for the color of the plot background. Default is \code{"white"}.
#' @param panel.grid.major.color A character string that specifies the HEX code for the color of the major grid lines. If left as \code{NULL} (the default setting), the color will be automatically set according to the \code{panel.background.fill} parameter.
#' @param legend.justification A numeric vector of length 2 that determines the justification of the legend. Default is \code{c(0, 1)}.
#' @param legend.key An \code{element_rect} object defining the appearance of the legend key. Default is \code{element_rect(fill = "transparent")}.
#' @param text A ggplot2 element specifying the appearance of text elements in the plot.
#' @param axis.text A ggplot2 element specifying the appearance of axis text in the plot.
#' @param strip.text.x A ggplot2 element specifying the appearance of strip text for x-axis facet labels.
#' @param plot.caption A ggplot2 element specifying the appearance of the plot caption.
#' @param legend.title A ggplot2 element specifying the appearance of the legend title.
#' @param axis.title A ggplot2 element specifying the appearance of axis titles.
#' @param plot.title A ggplot2 element specifying the appearance of the plot title.
#' @param plot.subtitle A ggplot2 element specifying the appearance of the plot subtitle.
#' @param legend.background A ggplot2 element specifying the appearance of the legend background.
#' @param ... Other arguments passed on to the function `theme()`.
#'
#' @return A ggplot2 theme object that can be added to a ggplot.
#'
#' @examples
#' ggplot(data = mtcars, aes(x = disp, y = mpg, color = qsec, size = wt)) +
#' geom_point() +
#' labs(y = "Miles per gallon",
#' x = "Engine size (cu. in.) ",
#' title = "Larger engine consume more gas",
#' subtitle = "V-shape engines are typically larger",
#' caption = "Data source: Motor Trend US magazine.",
#' color = "1/4 mile time (seconds)",
#' size = "Weight (1000 lbs)") +
#' scale_color_ul(palette = "red", discrete = FALSE) +
#' theme_ul(plot.background.fill = ul_color("lajt"))
#' @importFrom ggplot2 element_rect theme_gray theme element_blank element_line element_text
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
theme_ul <- function(legend.position = "top",
                     panel.background.fill = "#E8E9EA",
                     plot.background.fill = "white",
                     panel.grid.major.color = NULL,
                     legend.justification = c(0, 1),
                     legend.key = element_rect(fill = "transparent"),
                     text = element_text(colour = "#58595b"),
                     axis.text = element_text(colour = "#58595b"),
                     strip.text.x = element_text(colour = "#58595b"),
                     plot.caption = element_text(hjust = 1),
                     legend.title = element_text(face = "bold"),
                     axis.title = element_text(face="bold"),
                     plot.title = element_text(face="bold"),
                     plot.subtitle = element_text(hjust = 0),
                     legend.background = element_rect(fill="transparent", colour="transparent"),
                     ...){

  if (is.null(panel.grid.major.color)) {
    panel.grid.major.color <- ifelse(panel.background.fill == "white", yes = "#E8E9EA", no = "white")
  }

  theme_gray()
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = panel.grid.major.color),
    axis.ticks = element_blank(),
    # legend
    legend.justification = legend.justification,
    legend.background = legend.background,
    legend.key = legend.key,
    #text elements
    strip.text.x = strip.text.x,
    legend.position =  legend.position,
    legend.title = legend.title,
    axis.title = axis.title,
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    plot.caption = plot.caption,
    text = text,
    axis.text = axis.text,
    plot.background = element_rect(fill = plot.background.fill),
    panel.background = element_rect(fill = panel.background.fill), ...
  )
}
