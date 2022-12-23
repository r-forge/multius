#' Network evolution model with a prespecified blockmodel type and partition (symmetric networks with incomers and outgoers)
#'
#' @description Generate a symmetric network with a selected blockmodel type and partition 
#' with a specified number of incomers and outgoers. Considers local network mechanisms when
#' creating links within blocks.
#' @param X Initial binary network; of class \code{matrix}.
#' @param partition A desired partition in a vector format. Each unique value (positive integers) represents one cluster.
#' @param M Desired image matrix with block densities.
#' @param formula The list of local netork mechanisms to be considered.
#' @param theta A vector with the mechanisms' weights/strengths.
#' @param nin Number of incomers.
#' @param nout Number of outgoers.
#' @param minClusterSize Minimum cluster size.
#' @param k Number of iterations.
#' @param loops Wheter loops are allowed or not (default \code{FALSE}).
#' @param randomizeP The share of units to be randomly relocated between clusters.
#' @param randomSD The srandard deviation of a normal distribution form which the random part of weighed network statistics is sampled.
#' @return The list with the following elements:
#' \itemize{
#' \item \code{initialNetwork} - Initial network; of class \code{matrix}.
#' \item \code{finalNetwork} - Final (generated) network; of class \code{matrix}.
#' \item \code{initialPartition} - Initial partition.
#' \item \code{finalPartition} - Final partition (i.e., partition after randomization and after incomers and outgoers).
#' \item \code{M} - The desired (specified) image matrix.
#' \item \code{k} - The number of iterations.
#' \item \code{combinedPartitions} - Data frame with initial and final partition.
#' \item \code{whenIncomers} - A vector of which elements tells us at which iterations the incomers were added.
#' \item \code{whenOutgoers} - A vector of which elements tells us at which iterations the outgoers were removed.
#' \item \code{ERR} - Sum of squared differences between the desired and empirical densities across blocks; for each iteration.
#' \item \code{linkERR} - The difference in the number of links between the generated number of links and desired number of links; for each iteration.
#' }
#' @examples
#' formula <- list(mutuality, popularity, OTPtransitivity)
#' X <- matrix(sample(c(0,1), size = 9**2, replace = TRUE), nrow = 9)
#' diag(X) <- 0
#' M <- matrix(c(0.1, 0.8, 0.8, 0.1), nrow = 2)
#' partition <- c(1, 2, 2, 1, 1, 2, 2, 2, 1)
#' nemSymBMinout(X = X, 
#'               partition = partition, 
#'               formula = formula, 
#'               theta = c(1, 1, 1), 
#'               M = M, 
#'               k = 100, 
#'               minClusterSize = 2,
#'               nin = 10,
#'               nout = 5,
#'               loops = FALSE)
#' @author Marjan Cugmas and Aleš Žiberna
#' @export

nemSymBMinout <- function(X = X, 
                       partition = partition,
                       M = M,
                       formula = NULL,
                       theta = NULL,
                       nin = 5,
                       nout = 20,
                       minClusterSize = 5,
                       k = 1000,
                       loops = FALSE,
                       randomizeP = 0,
                       randomSD = 0.02){
  
  # Functions ---
  checkNames <- function(net, partition){
    if (is.null(rownames(net))) {
      rownames(net) <- colnames(net) <- paste0("T01_", sprintf("%04d", 1:nrow(net)))
      names(partition) <- rownames(net)
    }
    if (!all(substr(rownames(net), 1, 1) %in% "T")) stop("Check the names of units. The names must start with a letter T. Alternatively you can provide network and partition without names of the units.")
    return(list("net" = net, "partition" = partition))
  }
  
  currentT <- function(net) {
    text <- unlist(lapply(rownames(net), FUN = function(x) strsplit(x, "_")[[1]][[1]]))
    return(max(as.numeric(substr(text, 2, nchar(text)))))
  }
  
  linkError <- function(X, partition, M, loops) {
    groupSizes <- table(partition)
    minusDiagonal <- 'ifelse'(test = loops, yes = list(matrix(0, nrow = length(groupSizes), ncol = length(groupSizes))), no = list(diag(groupSizes)))[[1]]
    possibleLinks <- outer(groupSizes, groupSizes) - minusDiagonal
    idealNumberOfLinks <- M * possibleLinks
    empiricalNumberOfLinks <- funByBlocks(x = X, clu = partition, FUN = sum, ignore.diag = FALSE)
    
    return(empiricalNumberOfLinks - idealNumberOfLinks)
  }
  
  nameOfNewcomer <- function(X, nextT){
    timePoint <- sprintf("%02d", nextT)
    unitName <- sprintf("%04d",max(as.numeric(unlist(lapply(rownames(X), FUN = function(x) strsplit(x, "_")[[1]][[2]]))))+1)
    return(paste0("T", timePoint, "_", unitName))
  }
  # End functions ----
  
  newNames <- checkNames(net = X, partition = partition)
  X <- newNames$net
  partition <- newNames$partition
  nextT <- currentT(X) + 1
  
  partitionOld <- partition
  partition <- randomizePartition(partition, p = randomizeP, checkSelected = FALSE)
  
  if (sum(M[lower.tri(M)] != M[upper.tri(M)])>0) {stop("The desired image matrix (M) is not symmetric!")}
  
  initial0 <- X
  partition0 <- partition
  
  rownames(M) <- colnames(M) <- 1:nrow(M)
  
  inat <- sample(k, size = nin)
  outat <- sample(k, size = nout)
  
  linkErrorIt <- array(NA, dim = c(nrow(M), ncol(M), k))
  currentError <- NULL
  for (i in 1:k){
    
    egoName <- sample(rownames(X), size = 1)
    egoID <- which(rownames(X) %in% egoName)
    
    netStat <- WeightedNetworkStatistics(formula = formula, X = X, actor = egoID, theta = theta, randomSD = randomSD)
    decision <- chooseBlockRow(X = X, partition = partition, actor = egoID, M = M, loops = loops, randomBlock = "square")
    
    if (decision["sign"] == 0) decision["sign"] <- sample(c(-1, 1), size = 1)
    
    if (decision["sign"] > 0) {
      naVoljoID <- intersect(which(X[egoID, ] == 1), which(partition == decision["block"]))
      if (loops == FALSE) naVoljoID <- naVoljoID[naVoljoID != egoID]
      if (length(naVoljoID) > 0){
        netStatRand <- netStat[naVoljoID,] 
        alterID <- which(rownames(X) %in% sample(names(netStatRand[min(netStatRand) == netStatRand]), 1))
        X[egoID, alterID] <- 0; X[alterID, egoID] <- 0
      }
    }
    if (decision["sign"] < 0) {
      naVoljoID <- intersect(which(X[egoID, ] == 0), which(partition == decision["block"]))
      if (loops == FALSE) naVoljoID <- naVoljoID[naVoljoID != egoID]
      if (length(naVoljoID) > 0){
        netStatRand <- netStat[naVoljoID,] 
        
        pogoj <- TRUE
        altersID <- sort(netStatRand, decreasing = TRUE)
        stevec <- 1
        while (pogoj) {
          alterID <- which(rownames(X) %in% names(altersID[stevec]), 1)
          testa <- WeightedNetworkStatistics(formula = formula, theta = theta, X = X, actor = alterID, randomSD = 0)
          
          doesHeAcceptProbs <- (rank(testa, ties.method = "max")-1)/(nrow(testa) - 1)
          doesHeAccept <- as.logical(stats::rbinom(1, size = 1, prob = doesHeAcceptProbs[egoID]))
          if (doesHeAccept) {X[egoID, alterID] <- 1; X[alterID, egoID] <- 1; pogoj <- FALSE
          } else {stevec <- stevec + 1; pogoj <- (stevec < length(altersID))}
        }
      }
    }
    
    # IN
    if (is.element(i, set = inat)){
      tab <- table(partition)
      whichCluster <- as.integer(sample(names(tab), prob=sqrt(tab), size = 1))
      partition <- c(partition, whichCluster)
      names(partition)[length(partition)] <- nameOfNewcomer(X = X, nextT = nextT)
      X <- cbind(rbind(X, 0), 0)
      rownames(X) <- colnames(X) <- names(partition)
    }
    
    # OUT
    if (is.element(i, set = outat)){
      smallGroups <- names(which(table(partition) <= minClusterSize))
      partitionProbs <- partition
      partitionProbs[partition %in% as.numeric(smallGroups)] <- 0
      partitionProbs[!partition %in% as.numeric(smallGroups)] <- 1
      partitionProbs[!is.element(el = names(partition), set = names(partitionOld))] <- 0
      whooutID <- sample(1:length(partition), prob = partitionProbs, size = 1)
      X <- X[-whooutID, -whooutID]
      partition <- partition[-whooutID]
    }
    
    currentError[i] <- SSEblock(X = X, M = M, partition = partition, loops = loops)
    linkErrorIt[,,i] <- linkError(X = X, partition = partition, M = M, loops = loops)
  }
  
  # INCOMERS AND OUTGOERS
  p1 <- data.frame("ID" = names(partitionOld), "p1" = partitionOld)
  p2 <- data.frame("ID" = names(partition), "p2" = partition)
  p12 <- merge(p1, p2, all = TRUE, sort = FALSE)
  p12[is.na(p12[,2]),2] <- "in" 
  p12[is.na(p12[,3]),3] <- "out"  
  
  return(list(
    "initialNetwork" = initial0,
    "finalNetwork" = X,
    "initialPartition" = partitionOld,
    "finalPartition" = partition,
    "M" = M,
    "k" = k,
    "combinedPartitions" = p12,
    "whenIncomers" = inat,
    "whenOutgoers" = outat,
    "ERR" = currentError,
    "linkERR" = linkErrorIt
  ))
}
