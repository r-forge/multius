#' @title Retrieve Marjan color(s)
#'
#' @description This function returns hexadecimal color codes from the Marjan color palette.  
#'
#' @param ... The names of the colors: \code{'myBlue'}, \code{'myJungle'}, \code{'myGreen'}, \code{'myRed'}, and \code{'myOrange'}. If not specified, the whole palette will be returned.
#'
#' @return A named character vector representing the color(s) from the Marjan color palette. If no specific color was requested, the entire color palette is returned.
#'
#' @examples 
#' \dontrun{
#' marjan_color("myBlue", "myRed") # Returns the hexadecimal codes for "myBlue" and "myRed"
#' marjan_color() # Returns the entire Marjan color palette
#' }
#' 
#' @export
marjan_color <- function(...) {
  marjan_colors <- c(
    `myBlue`     = "#44546A",
    `myJungle` = "#06A484",
    `myGreen`     = "#9CBE53",
    `myRed`    = "#BC2F2B",
    `myOrange`   = "#F19A00")
  
  cols <- c(...)
  if (is.null(cols))
    return (marjan_colors)
  marjan_colors[cols]
}
