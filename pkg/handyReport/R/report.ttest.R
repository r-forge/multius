#' CReport the results of t-test
#'
#' @description The function reports the results of t-tests with one categorical variable and one or several numeric variables.
#' @param ordinalna The name of categorical variable. One variable can be considered.
#' @param intervalna The name of interval/numberic variable(s). A string or a list. Several numeric variables can be considered.
#' @param dataset The dataframe with categorical and numerical variables.
#' @param hedgesG Should Hedges' g statistics be computed?
#' @note Hedges' g statistics is a variation of Cohen's d, corrected for bias due to the sammple size.
#' @examples
#' report.ttest(ordinalna = "vs", intervalna = c("disp", "wt", "qsec"), dataset = mtcars)
#' @references
#' Hedges, L., & Olkin, I. (2014). Statistical methods for meta-analysis. Academic press.
#' @author Marjan Cugmas
#' @export

report.ttest <- function(ordinalna, intervalna, dataset, hedgesG = FALSE){
  if (is.factor(dataset[, which(names(dataset)==ordinalna)]) == FALSE){
    dataset[, which(names(dataset)==ordinalna)] <- as.factor(dataset[, which(names(dataset)==ordinalna)])
  }
  if (length(ordinalna) > 1) stop("More than one categorical variable is provided.")
  ravni <- levels(dataset[, which(names(dataset)==ordinalna)])
  nravni <- length(ravni)
  res <- matrix(NA, nrow = length(intervalna), ncol = 6 + nravni + as.numeric(hedgesG))
  for (i in 1:length(intervalna)) {
    data <- dataset[, c(which(names(dataset)==ordinalna),  which(names(dataset)==intervalna[i]))]
    means <- round(as.vector(by(data = data[, intervalna[i]], INDICES = data[, ordinalna], mean)), 2)
    res[i, 1:nravni] <- means
    model <- t.test(data[, intervalna[i]] ~ as.factor(data[, ordinalna]))
    res[i, 1+nravni] <- round(model$statistic, 2)
    res[i, 2+nravni] <- round(model$parameter, 2)
    res[i, 3+nravni] <- round(model$p.value, 3)
    res[i, 4+nravni] <- round(model$conf.int[1], 2)
    res[i, 5+nravni] <- round(model$conf.int[2], 2)
    res[i, 6+nravni] <- model$method
    if (hedgesG == TRUE) {
      res[i, 7+nravni] <- round(g(ordinal = dataset[, which(names(dataset)==ordinalna)],
                                  interval = dataset[, which(names(dataset)==intervalna[i])],
                                  correct = TRUE), 2)}
  }
  labele <- c(ravni, "t", "df", "p", "CI95 low", "CI95 high", "method", ifelse(hedgesG==TRUE, yes = "Hedges G", no = ""))
  labele <- labele[!labele %in% ""]
  colnames(res) <- labele
  rownames(res) <- intervalna
  return(res)
}
