#' Break a string
#'
#' @description The function breaks a string after around the specified number of characters.
#' @param x A string.
#' @param nChar The number of characters after which the new line is inserted. Default to 20.
#' @return A string with inserted \code{\\n}.
#' @examples
#' someText <- "This is the function that breaks a string."
#' breakString(x = someText, nChar = 20)
#' @author
#' Marjan Cugmas
#' @export

breakString <- function(x, nChar = 20){
  baseFunction <- function(x = x, nChar=nChar){
    x <- strsplit(x, " ")[[1]]
    besedilo <- NULL
    stevec <- 1
    for (i in 1:length(x)){
      #if (i == 1) besedilo[i] <- x[1]
      if (sum(nchar(besedilo)) >= nChar*stevec) {
        besedilo <- paste(besedilo, "\n", x[i])
        stevec <- stevec + 1
      } else {besedilo <- paste(besedilo, x[i])}
    }
    return(besedilo)
  }

  if (is.factor(x)) {
    labels <- trimws(unlist(lapply(levels(x), baseFunction, nChar = nChar)))
    x <- factor(x, labels = labels)
  } else {
    x <- trimws(unlist(lapply(x, baseFunction, nChar = nChar)))
  }
  return(x)
}
