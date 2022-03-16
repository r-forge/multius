#' Plot a solution of canonical correlations
#'
#' @description It plots the canonical solution that is obtained by the function \code{\link[multiUS:cancorPlus]{multiUS::cancorPlus}}.
#' @param ccRes The output of  \code{\link[multiUS:cancorPlus]{multiUS::cancorPlus}}.
#' @param xTitle The title of the first set of variables.
#' @param yTitle The title of the second set of variables.
#' @param inColors Whether plot should be plotted in colours (\code{TRUE}) (default) or in black and white (\code{FALSE}).
#' @param scaleLabelsFactor Parameter for setting the size of values (default is \code{1/2}). The size of plotted values is proportional to  its value to the power of \code{scaleLabelsFactor}.
#' @param what Whether to plot regression coefficients (\code{"reg"}) (default) or correlations (i.e., canonical structure loadings) (\code{"cor"}).
#' @param nDigits Number of decimal places.
#' @param mar Margins, default is \code{mar = c(1, 2, 1, 1)}, see \code{\link[graphics:par]{graphics::par}}.
#' @return It plots the plot.
#' @examples
#' tmp<-cancorPlus(x = mtcars[, c(1,2,3)], y = mtcars[, c(4,5, 6)], useCCApackage = TRUE)
#' plotCCA(tmp, scaleLabelsFactor = 1/2, what = "cor")
#' @author Marjan Cugmas
#' @export

plotCCA <- function(ccRes, xTitle = "X", yTitle = "Y", inColors = TRUE, scaleLabelsFactor = 1/2,
                    what = "reg", nDigits = 2, mar = c(1, 2, 1, 1)){
  # try(dev.off(), silent = TRUE)
  corr <- ccRes$cor
  if (what == "reg") {
    Xcoef <- ccRes$xcoef[,1:length(corr)]
    Ycoef <- ccRes$ycoef[,1:length(corr)]
  }
  if (what == "cor") {
    Xcoef <- ccRes$scores$corr.X.xscores[,1:length(corr)]
    Ycoef <- ccRes$scores$corr.Y.yscores[,1:length(corr)]
    if (is.null(Xcoef)){
      warning("There are no correlations between the canonical variables and measured variables in ccRes object. Regression coefficients are ploted instead. Set useCCApackage = TRUE to obtain these correlations.")
      Xcoef <- ccRes$xcoef[,1:length(corr)]
      Ycoef <- ccRes$ycoef[,1:length(corr)]
    }
  }

  nVarX <- nrow(Xcoef)
  nVarY <- nrow(Ycoef)
  nCorr <- length(corr)

  yL <- c(1, 1 + nVarX + nVarY + 3)
  xL <- c(0.9, max(nCorr, ncol(Ycoef)) + 1)

  if (is.null(rownames(ccRes$xcoef))) {namesVarX <- paste0("Var X", 1:nVarX)} else {namesVarX <- rownames(ccRes$xcoef)}
  if (is.null(rownames(ccRes$ycoef))) {namesVarY <- paste0("Var Y", 1:nVarY)} else {namesVarY <- rownames(ccRes$ycoef)}

  varNames <- c(namesVarX, "", "", "", namesVarY)

  barveX <- matrix(grDevices::rgb(red = 0, green = 0, blue = 0, maxColorValue = 1),
                   nrow = nVarX,
                   ncol = nCorr)
  barveY <- matrix(grDevices::rgb(red = 0, green = 0, blue = 0, maxColorValue = 1),
                   nrow = nVarY,
                   ncol = ncol(Ycoef))

  if (inColors == TRUE) {
    maxColor <- max(abs(c(as.vector(Xcoef), as.vector(Ycoef))))
    for (i in 1:nrow(barveX)){
      for (j in 1:ncol(barveX)){
        if (Xcoef[i,j] > 0) barveX[i,j] <- grDevices::rgb(red = 0, green = 0, blue = abs(Xcoef[i,j]), maxColorValue = maxColor)
        if (Xcoef[i,j] < 0) barveX[i,j] <- grDevices::rgb(red = abs(Xcoef[i,j]), green = 0, blue = 0, maxColorValue = maxColor)
      }
    }
    for (i in 1:nrow(barveY)){
      for (j in 1:ncol(barveY)){
        if (Ycoef[i,j] > 0) barveY[i,j] <- grDevices::rgb(red = 0, green = 0, blue = abs(Ycoef[i,j]), maxColorValue = maxColor)
        if (Ycoef[i,j] < 0) barveY[i,j] <- grDevices::rgb(red = abs(Ycoef[i,j]), green = 0, blue = 0, maxColorValue = maxColor)
      }
    }
  }

  graphics::par(mar = mar)
  plot("1", ylim = yL, xlim = xL, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  graphics::text(x = 2:xL[2], y = yL[2], labels = paste0("CC", 1:(xL[2]-1)), font = 2)
  graphics::text(x = 1, y = 1:length(varNames), labels = varNames, pos = 4, font = 2)
  graphics::text(x = rep(2:(ncol(Ycoef)+1), each = nVarY),
       y = rep((nVarX+1):((nVarX+nVarY))+3, 3),
       labels = format(round(Ycoef, nDigits), digits=2, nsmall=2), cex = abs(Ycoef)**scaleLabelsFactor+0.2,
       col = barveY, xpd = TRUE)
  graphics::text(x = rep(2:(nCorr+1), each = nVarX),
       y = rep(1:nVarX, nCorr),
       labels = format(round(Xcoef, nDigits), digits=2, nsmall=2), cex = abs(Xcoef)**scaleLabelsFactor+0.2,
       col = barveX, xpd = TRUE)

  graphics::arrows(x0 = rep(2:xL[2]), x1 = rep(2:xL[2]), y1 = nVarX+3, y0 = nVarX+1, length = 0.1)
  graphics::arrows(x0 = rep(2:xL[2]), x1 = rep(2:xL[2]), y1 = nVarX+1, y0 = nVarX+3, length = 0.1)
  graphics::text(x = 2:(ncol(Ycoef)+1), y = nVarX+2,
       labels = format(round(corr, nDigits), digits=2, nsmall=2),
       font = 2,
       col = ifelse(sign(corr), yes = "blue", no = "red"))

  graphics::text(x = 0.7, y = mean(1:nVarX), label = xTitle, xpd = TRUE, srt = 90)
  graphics::text(x = 0.7, y = mean(1:nVarY) + nVarX+3, label = yTitle, xpd = TRUE, srt = 90)
}
