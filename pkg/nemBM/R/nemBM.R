#' Network evolution model with a prespecified blockmodel type and partition
#'
#' @description Generates an asymmetric network with a selected blockmodel type and partition. 
#' Considers local network mechanisms when creating links within blocks. 
#' Does not enable considering incomers and outgoers.
#' @param X Initial binary network; of class \code{matrix}.
#' @param partition A desired partition in a vector format. Each unique value (positive integers) represents one cluster.
#' @param M Desired image matrix with block densities.
#' @param formula The list of local netork mechanisms to be considered.
#' @param theta A vector with the mechanisms' weights/strengths.
#' @param k The number of iterations.
#' @param loops Wheter loops are allowed or not (default \code{FALSE}).
#' @return The list with the following elements:
#' \itemize{
#' \item \code{initialNetwork} - Initial network; of class \code{matrix}.
#' \item \code{finalNetwork} - Final (generated) network; of class \code{matrix}.
#' \item \code{formula} - The list of functions that define mechanisms used.
#' \item \code{theta} - A vector with the mechanisms' weights/strengths used.
#' \item \code{ERR} - Sum of squared differences between the desired and empirical densities across blocks; for each iteration.
#' \item \code{iterations} - The number of iterations.
#' \item \code{loops} - Wheter loops were allowed.
#' \item \code{M} - The desired (specified) image matrix.
#' \item \code{partition} - The partition.
#' \item \code{density} - Network density at each iteration.
#' \item \code{timeElapsed} - Running time.
#' }
#' @examples
#' formula <- list(mutuality, popularity, OTPtransitivity)
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' diag(X) <- 0
#' M <- matrix(c(0.1, 0.8, 0.1, 0.5), nrow = 2)
#' partition <- c(1, 2, 2, 1, 1, 2, 2, 2, 1)
#' res <- nemBM(X = X, partition = partition, formula = formula, 
#' theta = c(1, 1, 1), M = M, k = 100, loops = FALSE)
#' @author Marjan Cugmas and Aleš Žiberna
#' @references 
#' Cugmas, M., & Žiberna, A. (2022). Approaches to blockmodeling dynamic networks: a Monte Carlo simulation study. Social Networks, in print.
#' @export

nemBM <- function(X = X,
                  partition,
                  M,
                  formula, 
                  theta,
                  k = 10000,
                  loops = FALSE){
  if (is.null(colnames(X))) colnames(X) <- 1:ncol(X)
  if (is.null(rownames(X))) rownames(X) <- 1:ncol(X)
  
  initial0 <- X
  densityTarget <- globalDensity(M = M, partition = partition)
  stevec <- 1
  currentError <- SSEblock(X = X, M = M, partition = partition, loops = loops)
  pogojZanka <- TRUE
  n <- nrow(X)
  netStatDF <- NULL
  courseLinks <- data.frame("ego" = NA, "alterLink" = NA, "alterNonlink" = NA)
  currentDensity <- mean(X)
  
  timeElapsed <- system.time({
    for (i in 1:k){
      # izbor enot
      actor <- sample(nrow(X), size = 1)

      courseLinks[stevec, "ego"] <- actor

      l.friends <- which(X[actor,] == 1)
      nonfriends <- which(X[actor,] == 0)

      q <- mean(M[partition[actor], `if`(loops, partition, partition[-actor])])

      netStat <- WeightedNetworkStatistics(X = X, formula = formula, theta = theta, actor = actor, randomSD = 0.2)
      
      # glede na gostote blokov
      # (approach == "blockERR")
      # blockCrit == "outdegree"
      decision <- chooseBlockRow(X = X, partition = partition, actor = actor, M = M, loops = loops, randomBlock = "square")

      if (decision["sign"] == 0) decision["sign"] <- sample(c(-1, 1), size = 1)
      if (decision["sign"] > 0) {
        naVoljo <- intersect(which(X[actor, ] == 1), which(partition == decision["block"]))
        if (loops == FALSE) naVoljo <- naVoljo[naVoljo != actor]
        if (length(naVoljo) > 0){
          netStatRand <- netStat[naVoljo,]
          alter <- as.numeric(sample(names(netStatRand[min(netStatRand) == netStatRand]), 1))
          X[actor, alter] <- 0
        }
      }
      if (decision["sign"] < 0) {
        naVoljo <- intersect(which(X[actor, ] == 0), which(partition == decision["block"]))
        if (loops == FALSE) naVoljo <- naVoljo[naVoljo != actor]
        if (length(naVoljo) > 0){
          netStatRand <- netStat[naVoljo,]
          alter <- as.numeric(sample(names(netStatRand[max(netStatRand) == netStatRand]), 1))
          X[actor, alter] <- 1
        }
      }

      currentError[i] <- SSEblock(X = X, M = M, partition = partition, loops = loops)
      currentDensity[i] <- mean(X)

    }
  })
  return(list("initialNetwork" = initial0,
              "finalNetwork" = X,
              "formula" = formula,
              "theta" = theta,
              "ERR" = currentError,
              "iterations" = k,
              "loops" = loops,
              "M" = M,
              "partition" = partition,
              "density" = currentDensity,
              "timeElapsed" = timeElapsed))
}
