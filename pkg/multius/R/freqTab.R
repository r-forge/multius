#' Create a frequency table
#'
#' @description The function creates a frequency table with percentages for the selected categorical variable.
#' @param x Vector with the values of a categorical variable.
#' @param dec Number of decimal places for percentages.
#' @param ... Arguments passed to other functions, see \code{table}.
#' @examples
#' freqTab(mtcars[,2], dec = 1)
#' @author Aleš Žiberna

freqTab<-function(x, dec=NULL, ...){
  tbl<-table(x,...)
  cumFreq<-cumsum(tbl)
  perc<-tbl/sum(tbl)*100
  cumPerc<-cumsum(perc)
  if(!is.null(dec)){
    perc<-round(perc,dec)
    cumPerc<-round(cumPerc,dec)
  }
  frekTab<-as.data.frame(cbind("Freq."=tbl,"Cum. freq."=cumFreq, "%"=perc,"Cum. %" = cumPerc))
  return(frekTab)
}
