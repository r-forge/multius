#' Report the distributions of several dichotomous variables
#'
#' This function reports the frequencies of a specified event in multiple variables of a dataset.
#' It converts the event of interest to numeric, calculates the frequency of the event,
#' the number of valid and invalid entries for each variable, and optionally attaches variable labels.
#'
#' @param data A data frame containing the variables to be analysed.
#' @param varNames A vector of strings representing the names of the variables in the data frame.
#' @param event A value (numeric or character) representing the event of interest to be reported.
#'              This event is transformed to numeric 1 in each specified variable.
#'              By default, event = 1.
#' @param attachLabels Logical, if \code{TRUE}, attempts to attach variable labels from the attributes of the data frame.
#'                     By default, \code{attachLabels = FALSE}.
#'
#' @return A data frame with columns representing:
#'         1) the frequency of the event,
#'         2) the percentage of the event,
#'         3) the number of valid (non-NA) entries,
#'         4) the number of invalid (NA) entries for each specified variable.
#'
#' @examples
#' data <- data.frame(A = c(1, 1, 2, NA), B = c(1, 2, 2, 1), C = c(NA, 1, 1, 1))
#' varNames <- c("A", "B", "C")
#' report.multipleChoice(data, varNames)
#' @author Marjan Cugmas
#' @export
report.multipleChoice <- function(data, varNames, event = 1, attachLabels = FALSE) {
  for (i in varNames) data[,i] <- as.numeric(data[,i]== event)
  frekvence <- colSums(data[,varNames], na.rm = TRUE)
  veljavne <- apply(data[,varNames], 2, FUN = function(x) sum(!is.na(x)))
  neveljavne <- apply(data[,varNames], 2, FUN = function(x) sum(is.na(x)))
  if (attachLabels) try({names(frekvence) <- attributes(data)$variable.labels[varNames]}, silent = TRUE)
  ret <- data.frame(frekvence,
                    round(frekvence/veljavne*100),
                    veljavne, check.names = FALSE,
                    neveljavne)
  colnames(ret) <- c(paste0("Freq. (", event, ")"),
                     paste0("% (", event, ")"),
                     "n", "NA")
  return(ret)
}
