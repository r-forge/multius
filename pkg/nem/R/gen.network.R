#' Generating binary netwrks with pre-specified leves of erros
#'
#' @description It generates the networks with a pre-specified levels of errors for each block.
#' @param type A \code{matrix} containg types of blocks \code{"nul"}, \code{"null"} or \code{"com"}.
#' @param errors A \code{matrix} containg levels of errors (between 0 and 1) for each block; a single value can be provided if all blocks are assumed to contain the same level of errors.
#' @param n Network size.
#' @param loop Set to \code{TRUE} to allow loops in a network or \code{FALSE} to forbid loops in a network.
#' @param symmetric Should the network be symmetric \code{TRUE} or asymmetric \code{FALSE}.
#' @param size Specify the size of each cluster; \code{"equal"} if all clusters are assumed to be of equal size; or specify the vector of sizes (the elements of the vector have sum to 1).
#' @return A network with selected blockmodel type and level of errors.
#' @details This function can be used to generate random networks with a given blockmodel type and specified block densities.
#' @author Marjan Cugmas
#' @export

gen.network <- function(type, errors, n = 40, symmetric = FALSE, loop = FALSE, size = "equal"){
  new.network <- matrix(NA, nrow = n, ncol = n)
  if (size == "equal"){
    n.for.each.block <- n/nrow(type)
    spodnje <- seq(from = 1, to = n, by = n.for.each.block)
    zgornje <- seq(from = 0, to = n, by = n.for.each.block)[-1]
  }
  if (length(size) > 1){
    if (sum(size) != 1) warning("The size should sum to 1.")
    if (nrow(type) != length(size)) warning("The size of each block has to be specified.")
    n.for.each.block <- n*size
    meje <- matrix(NA, nrow = 2, ncol = nrow(type))
    meje[1, 1] <- 1
    meje[2,  ] <- cumsum(n.for.each.block)
    meje[1, 2:ncol(meje)] <- cumsum(n.for.each.block)[1:(ncol(meje)-1)]+1
    spodnje <- meje[1,]
    zgornje <- meje[2,]
  }
  if (is.matrix(errors) == FALSE) errors <- matrix(errors, nrow = dim(type)[1], ncol = dim(type)[2])
  for (i in 1:nrow(type)){
    for (j in 1:ncol(type)){
      if (type[i, j] == "com"){
        n.of.cells <- length(spodnje[i]:zgornje[i])*length(spodnje[j]:zgornje[j])
        new.network[spodnje[i]:zgornje[i], spodnje[j]:zgornje[j]] <- matrix(sample(x = c(0, 1), size = n.of.cells, replace = TRUE, prob = c(errors[i,j], 1 - errors[i,j])), nrow = length(spodnje[i]:zgornje[i]), ncol = length(spodnje[j]:zgornje[j]))
      }
      if (type[i, j] %in% c("null", "nul")){
        n.of.cells <- length(spodnje[i]:zgornje[i])*length(spodnje[j]:zgornje[j])
        new.network[spodnje[i]:zgornje[i], spodnje[j]:zgornje[j]] <- matrix(sample(x = c(0, 1), size = n.of.cells, replace = TRUE, prob = c(1 - errors[i,j], errors[i,j])), nrow = length(spodnje[i]:zgornje[i]), ncol = length(spodnje[j]:zgornje[j]))
      }
    }
  }
  if (symmetric == TRUE) new.network[lower.tri(new.network)] = t(new.network)[lower.tri(new.network)]
  if (loop == FALSE) diag(new.network) <- 0
  return(new.network)
}
