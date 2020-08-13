<<<<<<< .mine
#' Create a contingency table
#'
#' @description The function print the contingency table with columns sums or rowsums.
#' @param x First categorical variable.
#' @param y Second categorical variable.
#' @param margin Index to generate margin for. If \code{NULL}, then simple probabilities (non-conditional) are reported.
#' @examples
#' report.propTable(x = mtcars$gear, y = mtcars$carb, margin = 1, digits = 2)
#' @author Marjan Cugmas
#' @export

report.propTable <- function(x, y, margin = NULL, digits = 1){
  ptableO <- prop.table(table(x, y), margin)
  if (is.null(margin)){
    ptable <- cbind(ptableO, rowSums(ptableO))
    ptable <- rbind(ptable, colSums(ptable))
    colnames(ptable) <- c(colnames(ptableO), "skupaj")
    rownames(ptable) <- c(rownames(ptableO), "skupaj")
  }
  if (is.null(margin) == FALSE) {
    if (margin == 1){
      ptable <- cbind(ptableO, rowSums(ptableO))
      colnames(ptable) <- c(colnames(ptableO), "skupaj")
    }
    if (margin == 2){
      ptable <- rbind(ptableO, colSums(ptableO))
      rownames(ptable) <- c(rownames(ptableO), "skupaj")
    }
  }
  ptable <- round(ptable*100, digits = digits)
  return(ptable)
}
||||||| .r0
=======
#' Create a contingency table
#'
#' @description The function print the contingency table with columns sums or rowsums.
#' @param x First categorical variable.
#' @param y Second categorical variable.
#' @param margin enak
#' @examples
#' report.propTable(x = mtcars$gear, y = mtcars$carb, margin = 1, digits = 2)
#' @author Marjan Cugmas
#' @export

report.propTable <- function(x, y, margin = NULL, digits = 1){
  ptableO <- prop.table(table(x, y), margin)
  if (is.null(margin)){
    ptable <- cbind(ptableO, rowSums(ptableO))
    ptable <- rbind(ptable, colSums(ptable))
    colnames(ptable) <- c(colnames(ptableO), "skupaj")
    rownames(ptable) <- c(rownames(ptableO), "skupaj")
  }
  if (is.null(margin) == FALSE) {
    if (margin == 1){
      ptable <- cbind(ptableO, rowSums(ptableO))
      colnames(ptable) <- c(colnames(ptableO), "skupaj")
    }
    if (margin == 2){
      ptable <- rbind(ptableO, colSums(ptableO))
      rownames(ptable) <- c(rownames(ptableO), "skupaj")
    }
  }
  ptable <- round(ptable*100, digits = digits)
  return(ptable)
}
>>>>>>> .r26
