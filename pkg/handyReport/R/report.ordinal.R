#' Report the distributions of ordinal variables
#'
#' This function reports statistics for ordinal variables in a dataset.
#' It calculates proportions or counts of each level, the total count of valid (non-NA) entries, mean, and standard deviation.
#'
#' @param ordVarNames A vector of strings representing the names of the ordinal variables in the dataset.
#' @param data A data frame containing the ordinal variables to be analysed.
#' @param type An integer indicating the type of the report:
#'             1 = proportions (default),
#'             2 = counts,
#'             3 = counts and proportions.
#' @param attachLabels Logical, if \code{TRUE}, attempts to attach variable labels from the attributes of the data frame.
#'                     By default, \code{attachLabels = FALSE}.
#' @param dec The number of decimal places to round the values.
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
#' @importFrom stats t.test aov chisq.test oneway.test sd t.test

report.ordinal <- function(ordVarNames, data, type = 1, dec = 1, attachLabels = FALSE){

  labele <- levels(data[, ordVarNames[1]])
  nravni <- length(labele)

  res <- matrix(NA, nrow = length(ordVarNames), ncol = nravni + 3)
  colnames(res) <- c(labele, "n", "mean", "sd")
  rownames(res) <- ordVarNames
  for (i in 1:length(ordVarNames)){
    if (type == 1)  res[i, 1:(nravni+1)] <- c(round(prop.table(table(data[,ordVarNames[i]]))*100, dec), sum(!is.na(data[,ordVarNames[i]])))
    if (type == 2) res[i, 1:(nravni+1)] <- c(table(data[, ordVarNames[i]]), sum(!is.na(data[,ordVarNames[i]])))
    if (type == 3) res[i, 1:(nravni+1)] <- c(paste0(table(data[, ordVarNames[i]]), " (", round(prop.table(table(data[,ordVarNames[i]]))*100, dec), ")"), sum(!is.na(data[,ordVarNames[i]])))
    res[i, (nravni+2):(nravni+3)] <- round(c(mean(as.numeric(data[, ordVarNames[i]]), na.rm = TRUE), sd(as.numeric(data[, ordVarNames[i]]), na.rm = TRUE)), digits = dec)
  }

  if (attachLabels == TRUE) try({rownames(res) <- attributes(data)$variable.labels[rownames(res)]}, silent = TRUE)

  return(res)
}
