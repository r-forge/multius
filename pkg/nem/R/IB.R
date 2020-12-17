#' The number of inconsistent blocks
#'
#' @description The function calculates the number of incosnstent blocks between two image matrices.
#' @param MI1 The first image matrix. The possible block types are \code{com}, \code{nul} and \code{dnc}.
#' @param MI2 The second image matrix. The possible block types are \code{com}, \code{nul} and \code{dnc}.
#' @return A list with \code{IB} which is a number of inconsistent blocks and \code{order} which is an order of
#' groups from the original image matrix, corresponding to the number of inconsistent blocks.
#' @author Marjan Cugmas
#' @export

IB <- function(IM1, IM2){
  try(IM1 <- IM1[, , ,], silent = TRUE)
  try(IM2 <- IM2[, , ,], silent = TRUE)

  if(is.element(el = FALSE, set = c(dim(IM1) == dim(IM2)))) warning("The image matrices must have the same dimensions!")

  IM2[IM2 == "null"] <- "nul"
  IM1[IM1 == "null"] <- "nul"
  k <- nrow(IM1)
  all.perms <- permn(1:k)
  diags <- sapply(1:length(all.perms), function(x) {
    sum((IM1[all.perms[[x]], all.perms[[x]]] != IM2) * (IM2 != "dnc"))
  })
  return(list("IB" = min(diags), "order" = all.perms[[which.min(diags)]]))
}
