#' KNN-imputation method
#'
#' @description Function that fills in all NA values using the k-nearest-neighbours of each case with NA values.
#' By default it uses the values of the neighbours and obtains an weighted (by the distance to the case) average of
#' their values to fill in the unknows. If \code{meth='median'} it uses the median/most frequent value, instead.
#' @param data 	A data frame with the data set.
#' @param k The number of nearest neighbours to use (defaults to 10).
#' @param scale Boolean setting if the data should be scale before finding the nearest neighbours (defaults to TRUE).
#' @param meth String indicating the method used to calculate the value to fill in each NA. Available values are \code{median} or \code{weighAvg} (the default).
#' @param distData Optionally you may sepecify here a data frame containing the data set that should be used to find the neighbours. This is usefull when filling in NA values on a test set, where you should use only information from the training set. This defaults to \code{NULL}, which means that the neighbours will be searched in data.
#' @details
#' This function uses the k-nearest neighbours to fill in the unknown (NA) values in a data set. For each case with any NA value it will search for its k most similar cases and use the values of these cases to fill in the unknowns.
#' If \code{meth='median'} the function will use either the median (in case of numeric variables) or the most frequent value (in case of factors), of the neighbours to fill in the NAs. If \code{meth='weighAvg'} the function will use a
#' weighted average of the values of the neighbours. The weights are given by \code{exp(-dist(k,x)} where \dist{dist(k,x)} is the euclidean distance between the case with NAs (x) and the neighbour k.
#' @note This is a slightly modified function from package \code{DMwR} by Luis Torgo. The modification allows the units with missing values at almost all variables.
#' @examples
#' mtcars$mpg[sample(1:nrow(mtcars), size = 5, replace = FALSE)] <- NA
#' knnImputation(data = mtcars)
#' @author Luis Torgo
#' @seealso \code{SeqKNN}
#' @references
#' Torgo, L. (2010) Data Mining using R: learning with case studies, CRC Press (ISBN: 9781439810187).

knnImputation <- function (data, k = 10, scale = TRUE, meth = "weighAvg", distData = NULL) {
  n <- nrow(data)
  if (!is.null(distData)) {
    distInit <- n + 1
    data <- rbind(data, distData)
  } else distInit <- 1
  N <- nrow(data)
  ncol <- ncol(data)
  nomAttrs <- rep(FALSE, ncol)
  for (i in seq(ncol)) nomAttrs[i] <- is.factor(data[, i])
  nomAttrs <- which(nomAttrs)
  hasNom <- length(nomAttrs)
  contAttrs <- setdiff(seq(ncol), nomAttrs)
  dm <- data
  if (scale) dm[, contAttrs] <- scale(dm[, contAttrs])
  if (hasNom) for (i in nomAttrs) dm[, i] <- as.integer(dm[, i])
  dm <- as.matrix(dm)
  nas <- which(!complete.cases(dm))
  if (!is.null(distData)) {tgt.nas <- nas[nas <= n]} else {tgt.nas <- nas}
  if (length(tgt.nas) == 0) warning("No case has missing values. Stopping as there is nothing to do.")
  xcomplete <- dm[setdiff(distInit:N, nas), ]
  if (nrow(xcomplete) < k) stop("Not sufficient complete cases for computing neighbors.")
  for (i in tgt.nas) {
    tgtAs <- which(is.na(dm[i, ]))
    dist <- scale(xcomplete, dm[i, ], FALSE)
    xnom <- setdiff(nomAttrs, tgtAs)
    if (length(xnom)) dist[, xnom] <- ifelse(dist[, xnom] > 0, 1, dist[, xnom])
    dist <- dist[, -tgtAs, drop=FALSE]
    dist <- sqrt(drop(dist^2 %*% rep(1, ncol(dist))))
    ks <- order(dist)[seq(k)]
    for (j in tgtAs) {
      if (meth == "median") {
        data[i, j] <- centralValue(data[setdiff(distInit:N, nas), j][ks])
      } else {data[i, j] <- centralValue(data[setdiff(distInit:N, nas), j][ks], exp(-dist[ks]))}
    }
  }
  data[1:n, ]
}

centralValue <- function (x, ws = NULL) {
  if (is.numeric(x)) {
    if (is.null(ws))
      median(x, na.rm = T)
    else if ((s <- sum(ws)) > 0)
      sum(x * (ws/s))
    else NA
  }
  else {
    x <- as.factor(x)
    if (is.null(ws))
      levels(x)[which.max(table(x))]
    else levels(x)[which.max(aggregate(ws, list(x), sum)[,
                                                         2])]
  }
}
