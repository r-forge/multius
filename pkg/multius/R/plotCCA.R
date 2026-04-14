#' Plot a canonical correlation analysis solution
#'
#' @description
#' Visualize the canonical correlation solution produced by
#' \code{\link[multiUS:cancorPlus]{multiUS::cancorPlus}}. The function displays
#' the first \code{show} canonical dimensions together with the corresponding
#' coefficients for the X and Y variable sets. You can plot either regression
#' coefficients (canonical weights) or canonical structure correlations, and
#' optionally color the printed values according to their sign and magnitude.
#'
#' Variable labels can be taken from row names of \code{ccRes$xcoef} and
#' \code{ccRes$ycoef}, replaced with \code{customNames}, or generated
#' automatically when row names are missing.
#'
#' @param ccRes The output of \code{\link[multiUS:cancorPlus]{multiUS::cancorPlus}}.
#' @param xTitle Character string used as the label for the first (X) set of variables.
#' @param yTitle Character string used as the label for the second (Y) set of variables.
#' @param inColors Logical; should values be shown in color (\code{TRUE}, default)
#'   or in black (\code{FALSE})? When \code{TRUE}, positive values are shown in
#'   blue tones and negative values in red tones, with intensity reflecting magnitude.
#' @param show Integer; number of canonical dimensions to display. If \code{NULL}
#'   (default), all canonical correlations in \code{ccRes$cor} are shown.
#' @param scaleLabelsFactor Numeric exponent controlling the size of the printed
#'   coefficients. Label size is proportional to
#'   \code{abs(value)^scaleLabelsFactor + 0.2}. Default is \code{1/2}.
#' @param what Character string specifying what should be plotted:
#'   \code{"reg"} for regression coefficients (default) or \code{"cor"} for
#'   canonical structure correlations. If \code{what = "cor"} but the required
#'   correlation components are unavailable in \code{ccRes$scores}, the function
#'   falls back to regression coefficients and issues a warning.
#' @param nDigits Integer; number of decimal places used when printing the values.
#' @param mar Numeric vector of plot margins passed to
#'   \code{\link[graphics:par]{graphics::par}}. Default is \code{c(1, 2, 1, 1)}.
#' @param gapLeft Integer; number of empty columns inserted before the first
#'   canonical dimension. Useful for aligning multiple plots. Default is \code{0}.
#' @param customNames Optional named character vector or list used to relabel
#'   variables. Names should correspond to variable names in
#'   \code{rownames(ccRes$xcoef)} and/or \code{rownames(ccRes$ycoef)}. If provided,
#'   matched variables are replaced by the supplied labels.
#'
#' @return This function is called for its side effect of drawing a plot and
#'   returns \code{NULL} invisibly.
#'
#' @examples
#' tmp <- cancorPlus(
#'   x = mtcars[, c(1, 2, 3)],
#'   y = mtcars[, c(4, 5, 6)],
#'   useCCApackage = TRUE
#' )
#'
#' plotCCA(
#'   tmp,
#'   scaleLabelsFactor = 1/2,
#'   what = "cor",
#'   customNames = c("cyl" = "Cylinders")
#' )
#'
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
  } else {                        # "cor"
    Xcoef <- ccRes$scores$corr.X.xscores[, 1:length(corr)]
    Ycoef <- ccRes$scores$corr.Y.yscores[, 1:length(corr)]
    if (is.null(Xcoef)){
      warning("No corr.* slots - falling back on regression coeffs.")
      Xcoef <- ccRes$xcoef[, 1:length(corr)]
      Ycoef <- ccRes$ycoef[, 1:length(corr)]
    }
  }
  
  if (!is.null(customNames)) {
    namesVarY <- as.vector(unlist(customNames[rownames(ccRes$ycoef)]))
    namesVarX <- as.vector(unlist(customNames[rownames(ccRes$xcoef)]))
  } 
  
  nVarX <- nrow(Xcoef); nVarY <- nrow(Ycoef)
  
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
                           grDevices::rgb(0, 0, abs(mat[i,j]), maxColorValue = maxC),
                           grDevices::rgb(abs(mat[i,j]), 0, 0, maxColorValue = maxC))
    out
  }
  barveX <- makeCols(Xcoef)
  barveY <- makeCols(Ycoef)
  
  startCol <- 2 + gapLeft                 
  xL <- c(0, max(ncol(Ycoef), length(corr)) + gapLeft + 1)
  yL <- c(1, 1 + nVarX + nVarY + 3)
  
  ## --------- plot ----------
  graphics::par(mar = mar)
  plot(1, type = "n", xlim = xL, ylim = yL, xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", bty = "n")
  
  ## headers
  graphics::text(x = startCol:xL[2], y = yL[2],
       labels = paste0("CC", seq_len(xL[2] - startCol + 1)), font = 2)
  
  ## variable labels (unchanged position)
  graphics::text(x = 1, y = seq_along(varNames), labels = varNames, pos = 4, font = 2)
  
  ## Y block numbers
  graphics::text(x = rep(startCol:(startCol + ncol(Ycoef) - 1), each = nVarY),
       y = rep((nVarX + 1):(nVarX + nVarY) + 3, 3),
       labels = format(round(Ycoef, nDigits), nsmall = 2),
       cex = abs(Ycoef)^scaleLabelsFactor + .2,
       col = barveY, xpd = TRUE)
  
  ## X block numbers
  graphics::text(x = rep(startCol:(startCol + length(corr) - 1), each = nVarX),
       y = rep(seq_len(nVarX), length(corr)),
       labels = format(round(Xcoef, nDigits), nsmall = 2),
       cex = abs(Xcoef)^scaleLabelsFactor + .2,
       col = barveX, xpd = TRUE)
  
  ## arrows between blocks
  graphics::arrows(x0 = rep(startCol:xL[2]), x1 = rep(startCol:xL[2]),
         y0 = nVarX + 1, y1 = nVarX + 3, length = .1)
  graphics::arrows(x0 = rep(startCol:xL[2]), x1 = rep(startCol:xL[2]),
         y0 = nVarX + 3, y1 = nVarX + 1, length = .1)
  
  ## canonical correlations row
  graphics::text(x = startCol:(startCol + length(corr) - 1), y = nVarX + 2,
       labels = format(round(corr, nDigits), nsmall = 2),
       font = 2, col = ifelse(corr >= 0, "blue", "red"))
  
  ## big X/Y labels
  graphics::text(x = 0.7, y = mean(seq_len(nVarX)),             label = xTitle, srt = 90, xpd = TRUE)
  graphics::text(x = 0.7, y = mean(seq_len(nVarY)) + nVarX + 3, label = yTitle, srt = 90, xpd = TRUE)
}