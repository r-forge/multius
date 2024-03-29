#' Report the ANOVA results
#'
#' This function conducts one-way ANOVAs for each of the provided interval variables, with a specified ordinal variable as a factor.
#' It reports group means for each level of the ordinal variable, ANOVA test statistics, and optionally, omega squared effect sizes.
#'
#' @param catVarName A string representing the name of the ordinal factor variable in the data frame.
#' @param numVarNames A vector of strings representing the names of the interval variables in the data frame.
#' @param data A data frame containing the variables to be analyzed.
#' @param omegaSq Logical, if \code{TRUE}, the function also calculates and reports omega squared effect sizes.
#'               By default, \code{omegaSq = FALSE}.
#'
#' @return A matrix with rows representing each specified interval variable, and columns representing:
#'         1) means for each level of the ordinal factor (one column per level),
#'         2) F-statistic,
#'         3) degrees of freedom 1 (df1),
#'         4) degrees of freedom 2 (df2),
#'         5) p-value,
#'         6) optionally, omega squared effect size.
#' @references
#' Kirk, R. E. (1996). Practical significance: A concept whose time has come. Educational and psychological measurement, 56(5), 746-759.
#' Tunks, T. (1978). The use of omega squared in interpreting statistical significance. Bulletin of the Council for Research in Music Education, 28-34.
#' @examples
#' report.anova(catVarName = "gear", numVarNames = c("mpg", "wt"),
#' data = mtcars, omegaSq = TRUE)
#' @author Marjan Cugmas
#' @importFrom stats t.test aov chisq.test oneway.test sd t.test
#' @export
report.anova <- function(catVarName, numVarNames, data, omegaSq = FALSE){
  if (is.factor(data[, which(names(data)==catVarName)]) == FALSE){
    data[, which(names(data)==catVarName)] <- as.factor(data[, which(names(data)==catVarName)])
  }
  if (length(catVarName) > 1) stop("More than one categorical variable is provided.")
  ravni <- levels(data[, which(names(data)==catVarName)])
  nravni <- length(ravni)
  res <- matrix(NA, nrow = length(numVarNames), ncol = 4 + nravni + as.numeric(omegaSq==TRUE))
  for (i in 1:length(numVarNames)) {
    tmp <- data[, c(which(names(data)==catVarName),  which(names(data)==numVarNames[i]))]
    means <- round(as.vector(by(data = tmp[, numVarNames[i]], INDICES = tmp[, catVarName], mean, na.rm = TRUE)), 2)
    res[i, 1:nravni] <- means

    model <- oneway.test(tmp[, numVarNames[i]] ~ as.factor(tmp[, catVarName]), var.equal = FALSE)

    res[i, 1+nravni] <- round(model$statistic, 2)
    res[i, 2+nravni] <- model$parameter[1]
    res[i, 3+nravni] <- round(model$parameter[2])
    res[i, 4+nravni] <- ifelse(model$p.value < 0.001, yes = "< 0.01", no = round(model$p.value, 3))

    if (omegaSq == TRUE) {
      modelAov <- summary(aov(tmp[, numVarNames[i]] ~ as.factor(tmp[, catVarName])))
      res[i, 4+nravni+  as.numeric(omegaSq==TRUE)] <- round((modelAov[[1]][1,2] - modelAov[[1]][1,1]*(modelAov[[1]][2,3]))/(modelAov[[1]][1,2] + modelAov[[1]][2,2] + modelAov[[1]][2,3]), 2)
    }
  }

  labele <- c(ravni, "F", "df1", "df2", "p")
  if (omegaSq==TRUE) labele <- c(labele, "Omega Sq")
  colnames(res) <- labele
  rownames(res) <- numVarNames
  return(res)
}
