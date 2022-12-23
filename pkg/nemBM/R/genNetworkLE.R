#' Relocating Links algorithm (RL algorithm)
#'
#' @description Generate network with a selected blockmodel and level or errors. See details section.
#' @param BM An image matrix of a blockmodel; of class \code{matrix} with possible values "nul" and "com".
#' @param LE Desired level of errors.
#' @param size A vector with the values specifying clusters' sizes. The number of elements of this vector must be the same as the number of clusters specified by an image matrix.
#' @param symmetric Wheter a symmetric network should be generated.
#' @return A binary network (of class \code{matrix}) with selected blockmodel type and level of errors.
#' @details 
#' The level of errors (LE) is used to simulate the extent of inconsistencies in blockmodels. It is defined on 
#' a scale between 0 and 1, where 0 corresponds to an ideal blockmodel, and 1 corresponds to a totally randomised 
#' network with the same density as in the ideal blockmodel.
#' @examples
#' cohesiveBM <- rbind(c("com", "nul"), c("nul", "com"))
#' network <- genNetworkLE(BM = cohesiveBM, LE = 0.5, size = c(5, 3))
#' @references 
#' Cugmas, M., Žiberna, A., & Ferligoj, A. (2021). The Relative Fit measure for evaluating a blockmodel. Statistical Methods & Applications, 30(5), 1315-1335.
#' @author Marjan Cugmas
#' @export

genNetworkLE <- function (BM = BM, LE = 0.4, size = NULL, symmetric = FALSE) {
  clustering <- rep(1:length(size), times = size)
  n <- sum(size)
  network <- matrix(NA, nrow = n, ncol = n)
  for (i in 1:nrow(BM)) {
    for (j in 1:ncol(BM)) {
      network[clustering == i, clustering == j] <- if (BM[i, j] == "com") {
        matrix(1, nrow = size[i], ncol = size[j])
      } else {matrix(0, nrow = size[i], ncol = size[j])}
    }
  }
  diag(network) <- -1
  initialNetwork <- network
  if (symmetric == FALSE) {
    n.relocated <- sum(initialNetwork == 0, na.rm = TRUE) *
      (sum(initialNetwork == 1, na.rm = TRUE)/(n * (n - 1)))
    n.relocated.rs <- n.relocated * LE
    p <- n.relocated.rs - floor(n.relocated.rs)
    n.relocated.rs <- floor(n.relocated.rs) + stats::rbinom(1, 1, p)
    remove.link <- sample(which(initialNetwork == 1), size = n.relocated.rs)
    make.link <- sample(which(initialNetwork == 0), size = n.relocated.rs)
    network[remove.link] <- 0
    network[make.link] <- 1
    diag(network) <- 0
    return(network)
  }
  if (symmetric == TRUE) {
    n.relocated <- sum(initialNetwork == 0, na.rm = TRUE) *
      (sum(initialNetwork == 1, na.rm = TRUE)/(n * (n - 1)))
    n.relocated.rs <- (n.relocated * LE)/2
    p <- n.relocated.rs - floor(n.relocated.rs)
    n.relocated.rs <- floor(n.relocated.rs) + stats::rbinom(1, 1, p)
    remove.link <- sample(which(lower.tri(initialNetwork) & initialNetwork == 1), size = n.relocated.rs)
    make.link <- sample(which(lower.tri(initialNetwork) & initialNetwork == 0), size = n.relocated.rs)
    network[remove.link] <- 0
    network[make.link] <- 1
    network[upper.tri(network)] = t(network)[upper.tri(network)]
    diag(network) <- 0
    return(network)
  }
}
