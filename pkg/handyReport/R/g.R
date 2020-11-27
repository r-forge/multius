#' Calculate the Hedge's g statistic
#'
#' @description The function calculate the Hedge's g statistics, which is the effect size measure.
#' @param ordinal The (binary) norminal or ordinal variable.
#' @param interval The numberical (interval) variable.
#' @param correct Wheter to apply correction for chance.
#' @examples
#' g(ordinal = mtcars$vs, interval = mtcars$gear, correct = TRUE)
#' @author Marjan Cugmas
#' @export

g <- function(ordinal, interval, correct = TRUE){
  gs <- diff(by(data = interval, ordinal, mean))/(sum((by(data = interval, ordinal, length) - 1)*by(data = interval, ordinal, sd))/(length(interval) - 2))

  if (correct == TRUE){
    a <- length(interval) - 2
    gs <- ((gamma(a/2))/(sqrt(a/2)*gamma((a-1)/2)))*gs
  }
  return(gs)
}
