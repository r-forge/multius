#' Relocating Links algorithm (RL algorithm)
#'
#' @description Generate network with a selected blockmodel and the level or errors.
#' @param BM An image matrix of a blockmodel.
#' @param LE Desired level of errors.
#' @param n Network size.
#' @param k Number of iterations.
#' @param size A vector with the values specifying the clusters' sizes (the elements must sum to 1).
#' @param symmetric Wheter a symmetric network should be generated.
#' @return A network with selected blockmodel type and level of errors.
#' @examples
#' # generate initial and ideal network
#' cohesiveBM <- rbind(c("com", "nul"), c("nul", "com"))
#' network <- gen.network.LE(BM = cohesiveBM, LE = 0.5, n = 12, size = rep(0.5, 2))
#' @author Marjan Cugmas
#' @export

gen.network.LE <- function (BM = BM, LE = 0.4, n = 24, size = rep(1/nrow(BM), nrow(BM)), symmetric = FALSE) {
  if (sum(size) != 1)
    warning("The sum of vector 'size' is not 1!")
  clustering <- rep(1:length(size), times = size * n)
  network <- matrix(NA, nrow = n, ncol = n)
  for (i in 1:nrow(BM)) {
    for (j in 1:ncol(BM)) {
      network[clustering == i, clustering == j] <- if (BM[i, j] == "com")
        matrix(1, nrow = size[i] * n, ncol = size[j] *  n)
      else matrix(0, nrow = size[i] * n, ncol = size[j] * n)
    }
  }
  diag(network) <- -1
  initialNetwork <- network
  if (symmetric == FALSE) {
    n.relocated <- sum(initialNetwork == 0, na.rm = TRUE) *
      (sum(initialNetwork == 1, na.rm = TRUE)/(n * (n - 1)))
    n.relocated.rs <- n.relocated * LE
    p <- n.relocated.rs - floor(n.relocated.rs)
    n.relocated.rs <- floor(n.relocated.rs) + rbinom(1, 1, p)
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
    n.relocated.rs <- floor(n.relocated.rs) + rbinom(1, 1, p)
    remove.link <- sample(which(lower.tri(initialNetwork) & initialNetwork == 1), size = n.relocated.rs)
    make.link <- sample(which(lower.tri(initialNetwork) & initialNetwork == 0), size = n.relocated.rs)
    network[remove.link] <- 0
    network[make.link] <- 1
    network[upper.tri(network)] = t(network)[upper.tri(network)]
    diag(network) <- 0
    return(network)
  }
}
