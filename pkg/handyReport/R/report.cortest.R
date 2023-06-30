#' Report the results of correlation test
#'
#' This function conducts correlation tests between pairs of interval variables.
#' It performs either Pearson or Spearman correlation tests, depending on the chosen method.
#'
#' @param varNamesX A vector of strings representing the names of the X variables in the data frame.
#' @param varNamesY A vector of strings representing the names of the Y variables in the data frame.
#' @param data A data frame containing the variables to be analyzed.
#' @param method A string indicating the method of correlation test to be conducted: \code{"pearson"} (default) or \code{"spearman"}.
#'
#' @return A matrix with rows representing each pair of variables, and columns representing:
#'         1) variable pair,
#'         2) test statistic,
#'         3) degrees of freedom (for Pearson's r),
#'         4) p-value,
#'         5) correlation coefficient (Pearson or Spearman, as specified),
#'         6) 95% confidence interval lower bound (for Pearson's r),
#'         7) 95% confidence interval upper bound (for Pearson's r).
#'
#' @examples
#' report.cortest(varNamesX = "drat", varNamesY = c("wt", "disp", "hp"), data = mtcars)
#' @author Marjan Cugmas
#' @importFrom stats t.test aov chisq.test oneway.test sd t.test cor.test
#' @export
report.cortest <- function(varNamesX, varNamesY, data, method = "pearson"){
  if (method == "pearson") {
    res <- matrix(NA, nrow = length(varNamesX)*length(varNamesY), ncol = 7)
    colnames(res) <- c("var", "t", "df", "p", "cor (Pearson)", "CI95 low", "CI95 high")
  }
  if (method == "spearman") {
    res <- matrix(NA, nrow = length(varNamesX)*length(varNamesY), ncol = 4)
    colnames(res) <- c("var", "S", "p", "cor (Spearman)")
  }
  stevec <- 1
  for (i in 1:length(varNamesX)) {
    for (j in 1:length(varNamesY)) {
      cor.res <- cor.test(x = as.numeric(data[, varNamesX[i]]), y = as.numeric(data[,varNamesY[j]]), method = method, exact = FALSE)
      if (method == "pearson"){
        res[stevec, ] <- c(paste0(varNamesX[i], " and ", varNamesY[j]),
                           round(cor.res$statistic, 2),
                           ifelse(is.null(cor.res$parameter), yes = "", no = cor.res$parameter),
                           ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)),
                           round(cor.res$estimate, 2),
                           ifelse(is.null(cor.res$conf.int[1]), yes = "", no = round(cor.res$conf.int[1], 2)),
                           ifelse(is.null(cor.res$conf.int[2]), yes = "", no = round(cor.res$conf.int[2], 2)))
      }
      if (method == "spearman"){
        res[stevec, ] <- c(paste0(varNamesX[i], " and ", varNamesY[j]),
                           round(cor.res$statistic, 2),
                           ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)),
                           round(cor.res$estimate, 2))
      }
      stevec <- stevec+1
    }
  }
  return(res)
}
