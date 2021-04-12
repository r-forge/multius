#' Rename variables
#'
#' @description The function for renaming one or several variables in a dataframe.
#' @param data A dataframe.
#' @param renames A list with oldnames and newnames (e.g, \code{list("oldname1" = "newname1", "oldname2" = "newname2")}).
#' @examples
#' renameVar(mtcars, list("cyl" = "Cylinders", "wt" = "Weight", "am" = "Transmission"))
#' @author Marjan Cugmas

renameVar <- function(data, renames){
  for (i in names(renames)) {
    colnames(data)[which(colnames(data) %in% i)] <- renames[i]
  }
  return(data)
}
