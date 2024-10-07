#' Report the distributions of ordinal variables
#'
#' This function reports statistics for ordinal variables in a dataset.
#' It calculates proportions or counts of each level, the total count of valid (non-NA) entries, mean, and standard deviation.
#'
#' @param ordVarNames A vector of strings representing the names of the ordinal variables in the dataset.
#' @param data A data frame or an object of class \code{survey.design} from \code{survey} package.
#' @param type An integer indicating the type of the report:
#'             1 = proportions (default),
#'             2 = counts,
#'             3 = counts and proportions.
#' @param attachLabels Logical, if \code{TRUE}, attempts to attach variable labels from the attributes of the data frame.
#'                     By default, \code{attachLabels = FALSE}.
#' @param dec The number of decimal places to round the values.
#' @param dec.freq Number of decimal places for frequencies. Defaults is \code{0}. Only used when an object of class \code{survey.design} is specified with the parameter \code{data}.
#' @param language The language used for displaying the statistics in the frequency table. This parameter accepts two values: \code{english} or \code{slovene}. Depending on the chosen language, all statistical terms and output will be adjusted accordingly. Default is \code{english}.
#'
#' @return A matrix with rows representing each specified ordinal variable, and columns representing:
#'         1) proportions or counts (or both) of each level (one column per level),
#'         2) total count of valid (non-NA) entries,
#'         3) mean,
#'         4) standard deviation.
#'
#' @examples
#' data <- data.frame(A = factor(c(1, 2, 3, 2), ordered = TRUE),
#'                    B = factor(c(2, 3, 2, 1), ordered = TRUE))
#' ordVarNames <- c("A", "B")
#' report.ordinal(ordVarNames, data, type = 1)
#' @author Marjan Cugmas
#' @export
#' @importFrom stats t.test aov chisq.test oneway.test sd t.test as.formula

report.ordinal <- function(ordVarNames, data, type = 1, dec = 1, dec.freq = 0, language = "english", attachLabels = FALSE){
  utezi <- any(class(data) %in% c("survey.design2", "survey.design"))

  if (!utezi) {
    labele <- levels(data[, ordVarNames[1]])
  } else {labele <- levels(data$variables[, ordVarNames[1]])}

  nravni <- length(labele)

  res <- matrix(NA, nrow = length(ordVarNames), ncol = nravni + 3)
  colnames(res) <- c(labele, "n", "mean", "sd")
  rownames(res) <- ordVarNames

  if (!utezi) {
    for (i in 1:length(ordVarNames)){
      if (type == 1)  res[i, 1:(nravni+1)] <- c(round(prop.table(table(data[,ordVarNames[i]]))*100, dec), sum(!is.na(data[,ordVarNames[i]])))
      if (type == 2) res[i, 1:(nravni+1)] <- c(table(data[, ordVarNames[i]]), sum(!is.na(data[,ordVarNames[i]])))
      if (type == 3) res[i, 1:(nravni+1)] <- c(paste0(table(data[, ordVarNames[i]]), " (", round(prop.table(table(data[,ordVarNames[i]]))*100, dec), ")"), sum(!is.na(data[,ordVarNames[i]])))
      res[i, (nravni+2):(nravni+3)] <- round(c(mean(as.numeric(as.character(data[, ordVarNames[i]])), na.rm = TRUE), sd(as.numeric(as.character(data[, ordVarNames[i]])), na.rm = TRUE)), digits = dec)
    }
  } else {
    for (i in 1:length(ordVarNames)){
      tmpCounts <- round(survey::svytable(stats::as.formula(paste("~", ordVarNames[i])), data), dec.freq)
      tmpPerc <- round(prop.table(tmpCounts)*100, dec)

      povprecje <- survey::svymean(stats::as.formula(paste("~as.numeric(as.character(", ordVarNames[i], "))")), data, na.rm = TRUE)[1]
      stdOdklon <- jtools::svysd(stats::as.formula(paste("~as.numeric(as.character(", ordVarNames[i], "))")), data, na.rm = TRUE)[1]

      if (type == 1)  res[i, 1:(nravni+1)] <- c(tmpPerc, sum(tmpCounts))
      if (type == 2) res[i, 1:(nravni+1)] <- c(tmpCounts, sum(tmpCounts))
      if (type == 3) res[i, 1:(nravni+1)] <- c(paste0(tmpCounts, " (", tmpPerc, " %)"), sum(tmpCounts))
      res[i, (nravni+2):(nravni+3)] <- round(c(povprecje, stdOdklon), digits = dec)
    }
  }

  if (language %in% c("Slovene", "slovene", "slo", "s")) {
    colnames(res)[colnames(res) %in% "mean"] <- "povpre\u010dje"
    colnames(res)[colnames(res) %in% "sd"] <- "standardni odklon"
  }

  if (attachLabels == TRUE){
    if (!utezi) {
      try({rownames(res) <- attributes(data)$variable.labels[rownames(res)]}, silent = TRUE)
    } else {
      try({rownames(res) <- attributes(data$variables)$variable.labels[rownames(res)]}, silent = TRUE)
    }
  }

  #if (!is.null(ncharBreakString)) rownames(res) <- breakString(rownames(res), nChar = ncharBreakString)

  return(res)
}
