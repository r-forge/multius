#' Sequential KNN imputation method
#'
#' @description This function estimates missing values sequentially from the units that has least missing rate, using weighted mean of k nearest neighbors.
#' @param data 	A data frame with the data set.
#' @param k The number of nearest neighbours to use (defaults to 10).
#' @details The function separates the dataset into an incomplete set with missing values and into a complete set without missing values.
#' The values in an incomplete set are imputed in the order of the number of missing values. A missing value is filled by the
#' weighted mean value of a corresponding column of the nearest neighbour units in the complete set. Once all missing values for
#' a given unit are imputed, the unit is moved into the complete set and used for the imputation of the rest of units in the
#' incomplete set. In this process, all missing values for one unit can be imputed simultaneously from the selected neighbour
#' units in the complete set. This reduces execution time from previously developed KNN method that selects nearest neighbours
#' for each imputation.
#' @note This is the function from package \code{SeqKNN} by Ki-Yeol Kim and Gwan-Su Yi.
#' @examples
#' mtcars$mpg[sample(1:nrow(mtcars), size = 5, replace = FALSE)] <- NA
#' seqKNNimp(data = mtcars)
#' @author Ki-Yeol Kim and Gwan-Su Yi
#' @seealso \code{KNNimp}
#' @references
#' Ki-Yeol Kim, Byoung-Jin Kim, Gwan-Su Yi (2004.Oct.26) "Reuse of imputed data in microarray analysis increases imputation efficiency", BMC Bioinformatics 5:160.

seqKNNimp <- function (data, k = 10) {
    x <- as.matrix(data)
    N <- dim(x)
    p <- N[2]
    N <- N[1]
    nas <- is.na(drop(x %*% rep(1, p)))
    xcomplete <- x[!nas, ]
    xbad <- x[nas, , drop = FALSE]
    missing<-c()

    for (i in seq(nrow(xbad))) {
      missing[i]<-sum(is.na(xbad[i,]))
    }
    missingorder<-order(missing)

    xnas <- is.na(xbad)
    xbadhat <- xbad
    cat(nrow(xbad), fill = TRUE)
    for (i in seq(nrow(xbad))) {
      j<-order(missingorder[i])
      xinas <- xnas[missingorder[i], ]
      xbadhat[missingorder[i], ] <- nnmiss(xcomplete, xbad[missingorder[i], ], xinas, K = k)
      xcomplete<-rbind(xcomplete, xbadhat[missingorder[i],])
    }
    x[nas, ] <- xbadhat
    x
  }

nnmiss <-
  function (x, xmiss, ismiss, K)
  {
    xd <- as.matrix(scale(x, xmiss, FALSE)[, !ismiss])
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    od <- order(dd)[seq(K)]

    od<-od[!is.na(od)]
    K<-length(od)

    distance<-dd[od]
    s<-sum(1/(distance+0.000000000000001))
    weight<-(1/(distance+0.000000000000001))/s
    xmiss[ismiss] <- drop(weight %*% x[od, ismiss, drop = FALSE]) ## weighted mean
    ##  xmiss[ismiss] <- drop(rep(1/K, K) %*% x[od, ismiss, drop = FALSE])  ## mean
    xmiss
  }

