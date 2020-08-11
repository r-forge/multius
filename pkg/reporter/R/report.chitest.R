#' CReport the results of chi-square test
#'
#' @description The function reports the results of several (with several variables) chi-square tests.
#' @param x The names of the first set of categorical variables.
#' @param y The names of the second set of categorical variables.
#' @param dataset The dataframe with categorical variables.
#' @param simulate.p.value Wheter to estimate the p-value by simulations (simulate.p.value=TRUE)  or analytically (simulate.p.value=FALSE).
#' @param cramer Wheter to calculate the Cramer's V coefficient.
#' @note One or several categorical variables can be set in x and/or x. When several variables are set, they have to be in a vector format.
#' @examples
#' report.chitest(x = c("cyl", "vs", "am"), y = "gear", dataset = mtcars, simulate.p.value = TRUE)
#' @author Marjan Cugmas
#' @export

report.chitest <- function(x, y, dataset, simulate.p.value = TRUE, cramer = FALSE, ...){
  res <- matrix(NA, nrow = length(x)*length(y), ncol = 2 +
                  as.numeric(simulate.p.value == TRUE) + as.numeric(simulate.p.value == FALSE) + as.numeric(simulate.p.value == FALSE) +
                  as.numeric(cramer == TRUE))

  basicNames <- c(c("var", "chi-square"),
                  'if'(simulate.p.value == TRUE, yes = "p", no = c("df", "p")),
                  ifelse(cramer == TRUE, yes = "Cr", no = ""))
  basicNames <- basicNames[!basicNames %in% ""]
  colnames(res) <- basicNames

  stevec <- 1
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      cor.res <- chisq.test(table(dataset[, x[i]], dataset[,y[j]]), simulate.p.value =  simulate.p.value, ...)
      if(simulate.p.value == TRUE){

        vrstica <- c(paste0(x[i], " and ", y[j]),
                     round(cor.res$statistic, 2),
                     ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)))
        if (cramer == TRUE) {
          vrstica <- c(vrstica, round((cor.res$statistic/sum(cor.res$observed))/min(nrow(cor.res$observed)-1, ncol(cor.res$observed)-1), 2))
        }
        res[stevec, ] <- vrstica
      }
      if(simulate.p.value == FALSE){
        vrstica <- c(paste0(x[i], " and ", y[j]),
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

