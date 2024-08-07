#' Report the results of correlation test
#'
#' This function conducts correlation tests between pairs of interval variables.
#' It performs either Pearson or Spearman correlation tests, depending on the chosen method.
#'
#' @param varNamesX A vector of strings representing the names of the X variables in the data frame.
#' @param varNamesY A vector of strings representing the names of the Y variables in the data frame.
#' @param data A data frame or an object of class \code{survey.design} from \code{survey} package.
#' @param method A string indicating the method of correlation test to be conducted: \code{"pearson"} (default) or \code{"spearman"}.
#' @param language The language used for displaying the statistics in the frequency table. This parameter accepts two values: \code{english} or \code{slovene}. Depending on the chosen language, all statistical terms and output will be adjusted accordingly. Default is \code{english}.
#' @param confLevel A numeric value between 0 and 1 indicating the confidence level for the confidence interval of the Pearson correlation. Default is \code{0.95}. This is ignored for Spearman correlation and when data is weighted.
#'
#' @return A matrix with rows representing each pair of variables, and columns representing:
#'         1) variable pair,
#'         4) p-value,
#'         5) correlation coefficient (Pearson or Spearman, as specified),
#'         6) 95% confidence interval lower bound (for Pearson's r),
#'         7) 95% confidence interval upper bound (for Pearson's r).
#'
#' @examples
#' report.cortest(varNamesX = "drat", varNamesY = c("wt", "disp", "hp"), data = mtcars)
#' @author Marjan Cugmas
#' @details The function calculates correlation coefficients, p-values, and, for Pearson method, confidence intervals if specified and applicable. For weighted survey data, confidence intervals are not calculated. Spearman correlation is available only for non-weighted data.
#' @importFrom stats t.test aov chisq.test oneway.test sd t.test cor.test
#' @importFrom jtools svycor
#' @export
report.cortest <- function(varNamesX, varNamesY, data, method = "pearson", confLevel = 0.95, language = "english"){
  # Confidence intervals only for pearson.
  # Confidence interval only for non-weighted data.
  # Spearman only for non-weighted data
  # if ((method == "spearman") & )

  if (language %in% c("Slovene", "slovene", "slo", "s")) {veznik <- "in"} else {veznik <- "and"}

  utezi <- any(class(data) %in% c("survey.design2", "survey.design"))

  confLevel <- ifelse(utezi, yes = FALSE, no = confLevel)

  if (method == "pearson") {
    res <- matrix(NA, nrow = length(varNamesX)*length(varNamesY), ncol = 3 + ifelse(!confLevel, yes = 0, no = 2))
    if (!confLevel) {
      colnames(res) <- c("var", "cor (Pearson)", "p")
    } else {
      colnames(res) <- c("var", "cor (Pearson)", "p", paste0("CI", round(confLevel*100), " low"), paste0("CI", round(confLevel*100), " high" ))
    }
  }
  if (method == "spearman") {
    res <- matrix(NA, nrow = length(varNamesX)*length(varNamesY), ncol = 3)
    colnames(res) <- c("var", "cor (Spearman)", "p")
  }

  stevec <- 1
  for (i in 1:length(varNamesX)) {
    for (j in 1:length(varNamesY)) {

      if (!utezi) {
        cor.res <- cor.test(x = as.numeric(data[, varNamesX[i]]), y = as.numeric(data[,varNamesY[j]]),
                            method = method,
                            exact = FALSE,
                            conf.level = ifelse(!confLevel, yes = 1, no = confLevel))
      } else {
        if (method == "pearson") {
          tmp <- jtools::svycor(stats::as.formula(paste0("~", varNamesX[i], "+", varNamesY[j])), design = data, digits = 4, sig.stats = TRUE)
          cor.res <- NULL
          cor.res$estimate <- tmp$cors[1,2]
          cor.res$p.value <- tmp$p.values[1,2]
        } else {
          cor.res <- NULL
          cor.res$estimate <- NA
          cor.res$p.value <- NA
        }

      }

      if (method == "pearson"){
        if (!confLevel) {
          res[stevec, ] <- c(paste0(varNamesX[i], " ", veznik, " ", varNamesY[j]),
                             round(cor.res$estimate, 2),
                             ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)))
        } else {
          res[stevec, ] <- c(paste0(varNamesX[i], " ", veznik, " ", varNamesY[j]),
                             round(cor.res$estimate, 2),
                             ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)),
                             ifelse(is.null(cor.res$conf.int[1]), yes = "", no = round(cor.res$conf.int[1], 2)),
                             ifelse(is.null(cor.res$conf.int[2]), yes = "", no = round(cor.res$conf.int[2], 2)))
        }

      }
      if (method == "spearman"){
        res[stevec, ] <- c(paste0(varNamesX[i], " ", veznik, " ", varNamesY[j]),
                           round(cor.res$estimate, 2),
                           ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)))
      }
      stevec <- stevec+1
    }
  }
  return(res)
}
