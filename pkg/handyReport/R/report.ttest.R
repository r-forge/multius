#' T-test Reporting Function
#'
#' @description This function performs a t-test on a given categorical variable and a set of numerical variables. It also optionally computes Hedges' G effect size and confidence intervals.
#'
#' @param catVarName A string. The name of the categorical variable to be used in the t-tests.
#' @param numVarNames A vector of strings. The names of the numerical variables to be used in the t-tests.
#' @param data A data frame or an object of class \code{survey.design} from \code{survey} package.
#' @param hedgesG A logical. If \code{TRUE}, Hedges' G effect size is computed for each t-test. Defaults to \code{FALSE}. Not implemented for weighted data.
#' @param CI A logical. If \code{TRUE}, confidence intervals are computed for each t-test. Defaults to \code{FALSE}.

#' @return A matrix with the results of the t-tests. Each row corresponds to one of the numerical variables,
#'         and the columns show the means for each level of the categorical variable, the t-statistic, the degrees of freedom,
#'         the p-value, and optionally the confidence intervals and Hedges' G effect size.
#'
#' @examples
#' report.ttest(catVarName = "am", c("mpg", "disp"), mtcars, hedgesG = TRUE, CI = TRUE)
#'
#' @export
#' @author Marjan Cugmas
#'
#' @note This function assumes that the categorical variable is a factor.
#'       If the variable is not a factor, it will be converted to a factor within the function.
#'
#' @seealso \code{\link{t.test}}

report.ttest <- function(catVarName, numVarNames, data, hedgesG = FALSE, CI = FALSE){
  # hedgesG not implemented for weighted data
  utezi <- any(class(data) %in% c("survey.design2", "survey.design"))
  if (!utezi) {
    if (is.factor(data[, which(names(data)==catVarName)]) == FALSE){
      data[, which(names(data)==catVarName)] <- as.factor(data[, which(names(data)==catVarName)])
    }
    if (length(catVarName) > 1) stop("More than one categorical variable is provided.")
    ravni <- levels(data[, which(names(data)==catVarName)])
    nravni <- length(ravni)
  }

  if (utezi) {
    if (is.factor(data[, which(names(data)==catVarName)]) == FALSE){
      data[, which(names(data$variables)==catVarName)] <- as.factor(data$variables[, which(names(data$variables)==catVarName)])
    }
    if (length(catVarName) > 1) stop("More than one categorical variable is provided.")
    ravni <- levels(data$variables[, which(names(data$variables)==catVarName)])
    nravni <- length(ravni)
  }

  hedgesG <- ifelse(utezi == TRUE, yes = FALSE, no = hedgesG)

  res <- matrix(NA, nrow = length(numVarNames), ncol = 3 + as.numeric(CI)*2 + nravni + as.numeric(hedgesG))
  for (i in 1:length(numVarNames)) {

    # Calculate means and t-test
    if (utezi){
      means <- round(survey::svyby(formula = stats::as.formula(paste("~", numVarNames[i])),
                           by = stats::as.formula(paste("~", catVarName)), FUN = survey::svymean, design = data)[,2], 2)
      model <- survey::svyttest(stats::as.formula(paste0(numVarNames[i], "~", catVarName)), data, survey::svymean, var.equal = FALSE)
    } else {
      tmp <- data[, c(which(names(data)==catVarName),  which(names(data)==numVarNames[i]))]
      means <- round(as.vector(by(data = tmp[, numVarNames[i]], INDICES = tmp[, catVarName], mean, na.rm = TRUE)), 2)
      model <- t.test(tmp[, numVarNames[i]] ~ as.factor(data[, catVarName]), var.equal = FALSE)
    }
    res[i, 1:nravni] <- means
    res[i, 1+nravni] <- round(model$statistic, 2)
    res[i, 2+nravni] <- round(model$parameter, 2)
    res[i, 3+nravni] <- ifelse(model$p.value < 0.001, yes = "< 0.01", no = round(model$p.value, 3))
    if (CI) {
      res[i, 4+nravni] <- round(model$conf.int[1], 2)
      res[i, 5+nravni] <- round(model$conf.int[2], 2)
    }
    if (hedgesG) {
      res[i, ncol(res)] <- round(g(ordinal = tmp[, which(names(tmp)==catVarName)],
                                   interval = tmp[, which(names(tmp)==numVarNames[i])],
                                   correct = TRUE), 2)
    }
  }
  if (CI) labele <- c(ravni, "t", "df", "p", "CI95 low", "CI95 high", ifelse(hedgesG==TRUE, yes = "Hedges G", no = ""))
  if (!CI) labele <- c(ravni, "t", "df", "p",  ifelse(hedgesG==TRUE, yes = "Hedges G", no = ""))
  labele <- labele[!labele %in% ""]
  colnames(res) <- labele
  rownames(res) <- numVarNames
  return(res)
}
