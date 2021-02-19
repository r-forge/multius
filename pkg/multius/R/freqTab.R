#' Create a frequency table
#'
#' @description The function creates a frequency table with percentages for the selected categorical variable.
#' @param x Vector with the values of a categorical variable.
#' @param dec Number of decimal places for percentages.
#' @param cum wheter to calculate cummulative frequencies and percentages (default \code{TRUE}).
#' @param \dots Arguments passed to function \code{table}.
#' @examples
#' freqTab(mtcars[,2], dec = 1)
#' @author Aleš Žiberna

freqTab <- function (x, dec = 2, cum = TRUE, ...){
  tbl <- table(x, ...)
  if (cum)  cumFreq <- cumsum(tbl)
  perc <- tbl/sum(tbl) * 100
  if (cum) cumPerc <- cumsum(perc)
  if (!is.null(dec)) {
    perc <- round(perc, dec)
    if (cum) cumPerc <- round(cumPerc, dec)
  }

  if (cum) frekTab <- as.data.frame(cbind(Freq. = tbl, `Cum. freq.` = cumFreq, `%` = perc, `Cum. %` = cumPerc))
  if (!cum) frekTab <- as.data.frame(cbind(Freq. = tbl, `%` = perc))
  return(frekTab)
}
