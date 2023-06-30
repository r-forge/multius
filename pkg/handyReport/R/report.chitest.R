#' Report the results of chi-square test
#'
#' This function conducts Chi-square tests for independence between pairs of categorical variables.
#' It provides the option to use Monte Carlo simulations for the p-value computation and to calculate Cramer's V.
#'
#' @param varNamesX A vector of strings representing the names of the X variables in the data frame.
#' @param varNamesY A vector of strings representing the names of the Y variables in the data frame.
#' @param data A data frame containing the variables to be analyzed.
#' @param simulate.p.value A logical value indicating whether to compute p-values by Monte Carlo simulation. Default is TRUE.
#' @param cramer A logical value indicating whether to compute Cramer's V for the pairs. Default is FALSE.
#' @param ... Additional arguments to be passed to the chisq.test function.
#'
#' @return A matrix with rows representing each pair of variables, and columns representing:
#'         1) variable pair,
#'         2) Chi-square statistic,
#'         3) p-value (calculated by simulation or classic method, based on 'simulate.p.value'),
#'         4) degrees of freedom (if 'simulate.p.value' is FALSE),
#'         5) Cramer's V (if 'cramer' is TRUE).
#'
#' @examples
#' report.chitest(varNamesX = c("cyl", "vs", "am"),
#' varNamesY = "gear", data = mtcars, simulate.p.value = TRUE)
#' @author Marjan Cugmas
#' @export
report.chitest <- function(varNamesX, varNamesY, data, simulate.p.value = TRUE, cramer = FALSE, ...){
  res <- matrix(NA, nrow = length(varNamesX)*length(varNamesY), ncol = 2 +
                  as.numeric(simulate.p.value == TRUE) + as.numeric(simulate.p.value == FALSE) + as.numeric(simulate.p.value == FALSE) +
                  as.numeric(cramer == TRUE))

  basicNames <- c(c("var", "chi-square"),
                  'if'(simulate.p.value == TRUE, yes = "p", no = c("df", "p")),
                  ifelse(cramer == TRUE, yes = "Cr", no = ""))
  basicNames <- basicNames[!basicNames %in% ""]
  colnames(res) <- basicNames

  stevec <- 1
  for (i in 1:length(varNamesX)) {
    for (j in 1:length(varNamesY)) {
      cor.res <- chisq.test(table(data[, varNamesX[i]], data[,varNamesY[j]]), simulate.p.value =  simulate.p.value, ...)
      if(simulate.p.value == TRUE){

        vrstica <- c(paste0(varNamesX[i], " and ", varNamesY[j]),
                     round(cor.res$statistic, 2),
                     ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)))
        if (cramer == TRUE) {
          vrstica <- c(vrstica, round((cor.res$statistic/sum(cor.res$observed))/min(nrow(cor.res$observed)-1, ncol(cor.res$observed)-1), 2))
        }
        res[stevec, ] <- vrstica
      }
      if(simulate.p.value == FALSE){
        vrstica <- c(paste0(varNamesX[i], " and ", varNamesY[j]),
                     round(cor.res$statistic, 2),
                     cor.res$parameter,
                     ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)))
        if (cramer == TRUE) {
          vrstica <- c(vrstica, round((cor.res$statistic/sum(cor.res$observed))/min(nrow(cor.res$observed)-1, ncol(cor.res$observed)-1), 2))
        }
        res[stevec, ] <- vrstica
      }
      stevec <- stevec+1
    }
  }
  return(res)
}

