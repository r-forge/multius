#' Plot a solution of canonical correlations
#'
#' @description
#' Plot the canonical solution produced by \code{\link[multiUS:cancorPlus]{multiUS::cancorPlus}}.
#' You can display either regression coefficients (canonical weights) or canonical
#' structure correlations for the first \code{show} canonical dimensions, with
#' optional color-coding by sign and magnitude.
#'
#' @param ccRes The output of \code{\link[multiUS:cancorPlus]{multiUS::cancorPlus}}.
#' @param xTitle Title for the first (X) set of variables.
#' @param yTitle Title for the second (Y) set of variables.
#' @param inColors Logical; plot values in color (\code{TRUE}, default) or in black
#'   and white (\code{FALSE}). Positive values map to blue tones, negative to red.
#' @param show Integer; how many canonical dimensions to display. Defaults to
#'   \code{length(ccRes$cor)}.
#' @param scaleLabelsFactor Numeric scaling exponent for the printed coefficients
#'   (default \code{1/2}). The label size is proportional to \code{|value|^scaleLabelsFactor}.
#' @param what What to plot: regression coefficients \code{"reg"} (default) or
#'   canonical structure correlations \code{"cor"}. If correlations are unavailable
#'   in \code{ccRes$scores}, the function falls back to regression coefficients
#'   with a warning.
#' @param nDigits Integer; number of decimal places shown for printed values.
#' @param mar Numeric vector of plot margins; passed to \code{\link[graphics:par]{graphics::par}}.
#'   Default is \code{c(1, 2, 1, 1)}.
#' @param gapLeft Integer; number of empty column gaps to insert before the first
#'   canonical dimension (useful for aligning multiple panels). Default \code{0}.
#' @param customNames Optional named character vector for relabeling variables.
#'   Names should match \code{rownames(ccRes$xcoef)} and/or \code{rownames(ccRes$ycoef)};
#'   values are the labels to display. Unmatched variables keep their original names
#'   (or default to \code{"Var X#"/"Var Y#"} when row names are missing).
#'
#' @return Draws the plot.
#'
#' @examples
#' tmp<-cancorPlus(x = mtcars[, c(1,2,3)], y = mtcars[, c(4,5, 6)], useCCApackage = TRUE)
#' plotCCA(tmp, scaleLabelsFactor = 1/2, what = "cor", customNames = c("cyl" = "Cylinders"))
#' @author Marjan Cugmas
#' @export

plotCCA <- function (ccRes,
                     xTitle = "X",
                     yTitle = "Y",
                     inColors = TRUE, show = NULL,
                     scaleLabelsFactor = 1/2,
                     what = "reg",
                     nDigits = 2,
                     mar = c(1, 2, 1, 1),
                     gapLeft = 0,
                     customNames = NULL
)
{
  if (is.null(show)) show <- length(ccRes$cor)
  corr  <- ccRes$cor[1:show]

  if (what == "reg"){
    Xcoef <- ccRes$xcoef[, 1:length(corr)]
    Ycoef <- ccRes$ycoef[, 1:length(corr)]
  } else {
    Xcoef <- ccRes$scores$corr.X.xscores[, 1:length(corr)]
    Ycoef <- ccRes$scores$corr.Y.yscores[, 1:length(corr)]
    if (is.null(Xcoef)){
      warning("No corr.* slots-falling back on regression coeffs.")
      Xcoef <- ccRes$xcoef[, 1:length(corr)]
      Ycoef <- ccRes$ycoef[, 1:length(corr)]
    }
  }

  nVarX <- nrow(Xcoef); nVarY <- nrow(Ycoef)

  if (!is.null(customNames)) {
    namesVarY <- setNames(rownames(ccRes$ycoef), rownames(ccRes$ycoef))
    namesVarX <- setNames(rownames(ccRes$xcoef), rownames(ccRes$xcoef))

    overlapY <- intersect(names(namesVarY), names(customNames))
    namesVarY[overlapY] <- customNames[overlapY]

    overlapX <- intersect(names(namesVarX), names(customNames))
    namesVarX[overlapX] <- customNames[overlapX]
  }

  if (is.null(rownames(ccRes$xcoef))) {
    namesVarX <- paste0("Var X", 1:nVarX)
  }

  if (is.null(rownames(ccRes$ycoef))) {
    namesVarY <- paste0("Var Y", 1:nVarY)
  }

  if (is.null(customNames) & (!is.null(rownames(ccRes$ycoef)))) {
    namesVarY <- rownames(ccRes$ycoef)
  }

  if (is.null(customNames) & (!is.null(rownames(ccRes$xcoef)))) {
    namesVarX <- rownames(ccRes$xcoef)
  }

  varNames  <- c(namesVarX, "", "", "", namesVarY)

  makeCols <- function(mat){
    out <- matrix("black", nrow(mat), ncol(mat))
    if (!inColors) return(out)
    maxC <- max(abs(mat))
    for (i in seq_len(nrow(mat)))
      for (j in seq_len(ncol(mat)))
        out[i,j] <- ifelse(mat[i,j] >= 0,
                           rgb(0, 0, abs(mat[i,j]), maxColorValue = maxC),
                           rgb(abs(mat[i,j]), 0, 0, maxColorValue = maxC))
    out
  }
  barveX <- makeCols(Xcoef)
  barveY <- makeCols(Ycoef)

  startCol <- 2 + gapLeft
  xL <- c(0, max(ncol(Ycoef), length(corr)) + gapLeft + 1)
  yL <- c(1, 1 + nVarX + nVarY + 3)

  ## --------- plot ----------
  par(mar = mar)
  plot(1, type = "n", xlim = xL, ylim = yL, xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", bty = "n")

  ## headers
  text(x = startCol:xL[2], y = yL[2],
       labels = paste0("CC", seq_len(xL[2] - startCol + 1)), font = 2)

  ## variable labels (unchanged position)
  text(x = 1, y = seq_along(varNames), labels = varNames, pos = 4, font = 2)

  ## Y block numbers
  text(x = rep(startCol:(startCol + ncol(Ycoef) - 1), each = nVarY),
       y = rep((nVarX + 1):(nVarX + nVarY) + 3, 3),
       labels = format(round(Ycoef, nDigits), nsmall = 2),
       cex = abs(Ycoef)^scaleLabelsFactor + .2,
       col = barveY, xpd = TRUE)

  ## X block numbers
  text(x = rep(startCol:(startCol + length(corr) - 1), each = nVarX),
       y = rep(seq_len(nVarX), length(corr)),
       labels = format(round(Xcoef, nDigits), nsmall = 2),
       cex = abs(Xcoef)^scaleLabelsFactor + .2,
       col = barveX, xpd = TRUE)

  ## arrows between blocks
  arrows(x0 = rep(startCol:xL[2]), x1 = rep(startCol:xL[2]),
         y0 = nVarX + 1, y1 = nVarX + 3, length = .1)
  arrows(x0 = rep(startCol:xL[2]), x1 = rep(startCol:xL[2]),
         y0 = nVarX + 3, y1 = nVarX + 1, length = .1)

  ## canonical correlations row
  text(x = startCol:(startCol + length(corr) - 1), y = nVarX + 2,
       labels = format(round(corr, nDigits), nsmall = 2),
       font = 2, col = ifelse(corr >= 0, "blue", "red"))

  ## big X/Y labels
  text(x = 0.7, y = mean(seq_len(nVarX)),             label = xTitle, srt = 90, xpd = TRUE)
  text(x = 0.7, y = mean(seq_len(nVarY)) + nVarX + 3, label = yTitle, srt = 90, xpd = TRUE)
}
