#' Predict the values of a categorical variable based on a linear discriminant function
#'
#' @description The function predicts the values of a categorical variable based on a linear discriminat function.
#' @param object Object obtained by the \code{ldaPlus} function or by the \code{MASS::lda}.
#' @param newdata New dataset (without categorical variable).
#' @param prior Prior probabilities of class membership to be used to predict values.
#' @param dimen The number of dimensions/linear discriminant functions to use. Defaults to all.
#' @param method Possible values are \code{plug-in}, \code{predictive} and \code{debiased}.
#' @param betweenGroupsWeights The proportions/weights used when computing the grand/total mean from group means.
#' @return
#' \itemize{
#' \item \code{class} - Predicted values of categorical variable.
#' \item \code{posterior} - Posterior probabilities (the values of the Fisher's calsification linear discrimination function).
#' \item \code{x} - Estimated values of discriminat function(s) for each unit.
#' }
#' @seealso \code{MASS::predict}
#' @examples
#' # Use the first 20 cars to estimate the model and the rest of cars to predict
#' # (for each car) wheter it has a V-shape engine or a straight engine.
#' ldaCars <- ldaPlus(x = mtcars[1:20,c(1, 2, 4, 5, 6)], grouping = mtcars[1:20,8])
#' predict.lda(object = ldaCars, newdata = mtcars[20:32,c(1, 2, 4, 5, 6)])
#' @author Aleš Žiberna
#' @usage
#' predict.lda(object, newdata, prior = object$prior, dimen,
#' method = c("plug-in", "predictive", "debiased"),
#' betweenGroupsWeights = object$betweenGroupsWeights)

predict.lda<- function (object, newdata, prior = object$prior, dimen,
                        method = c("plug-in", "predictive", "debiased"),
                        betweenGroupsWeights=object$betweenGroupsWeights) {
  if (!inherits(object, "lda"))
    stop("object not of class \"lda\"")
  if (!is.null(Terms <- object$terms)) {
    Terms <- delete.response(Terms)
    if (missing(newdata))
      newdata <- model.frame(object)
    else {
      newdata <- model.frame(Terms, newdata, na.action = na.pass,
                             xlev = object$xlevels)
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, newdata)
    }
    x <- model.matrix(Terms, newdata, contrasts = object$contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    if (xint > 0L)
      x <- x[, -xint, drop = FALSE]
  }
  else {
    if (missing(newdata)) {
      if (!is.null(sub <- object$call$subset))
        newdata <- eval.parent(parse(text = paste(deparse(object$call$x,
                                                          backtick = TRUE), "[", deparse(sub, backtick = TRUE),
                                                  ",]")))
      else newdata <- eval.parent(object$call$x)
      if (!is.null(nas <- object$call$na.action))
        newdata <- eval(call(nas, newdata))
    }
    if (is.null(dim(newdata)))
      dim(newdata) <- c(1L, length(newdata))
    x <- as.matrix(newdata)
  }
  if (ncol(x) != ncol(object$means))
    stop("wrong number of variables")
  if (length(colnames(x)) > 0L && any(colnames(x) != dimnames(object$means)[[2L]]))
    warning("variable names in 'newdata' do not match those in 'object'")
  ng <- length(object$prior)
  if (!missing(prior)) {
    if (any(prior < 0) || round(sum(prior), 5) != 1)
      stop("invalid 'prior'")
    if (length(prior) != ng)
      stop("'prior' is of incorrect length")
  }

  if(is.null(betweenGroupsWeights))betweenGroupsWeights<-prior
  means <- colSums(betweenGroupsWeights * object$means)
  scaling <- object$scaling
  x <- scale(x, center = means, scale = FALSE) %*% scaling
  dm <- scale(object$means, center = means, scale = FALSE) %*%
    scaling
  method <- match.arg(method)
  dimen <- if (missing(dimen)) {
    length(object$svd)
  }else min(dimen, length(object$svd))
  N <- object$N
  if (method == "plug-in") {
    dm <- dm[, 1L:dimen, drop = FALSE]
    dist <- matrix(0.5 * rowSums(dm^2) - log(prior), nrow(x),
                   length(prior), byrow = TRUE) - x[, 1L:dimen, drop = FALSE] %*%
      t(dm)
    dist <- exp(-(dist - apply(dist, 1L, min, na.rm = TRUE)))
  }
  else if (method == "debiased") {
    dm <- dm[, 1L:dimen, drop = FALSE]
    dist <- matrix(0.5 * rowSums(dm^2), nrow(x), ng, byrow = TRUE) -
      x[, 1L:dimen, drop = FALSE] %*% t(dm)
    dist <- (N - ng - dimen - 1)/(N - ng) * dist - matrix(log(prior) -
                                                            dimen/object$counts, nrow(x), ng, byrow = TRUE)
    dist <- exp(-(dist - apply(dist, 1L, min, na.rm = TRUE)))
  }
  else {
    dist <- matrix(0, nrow = nrow(x), ncol = ng)
    p <- ncol(object$means)
    X <- x * sqrt(N/(N - ng))
    for (i in 1L:ng) {
      nk <- object$counts[i]
      dev <- scale(X, center = dm[i, ], scale = FALSE)
      dev <- 1 + rowSums(dev^2) * nk/(N * (nk + 1))
      dist[, i] <- prior[i] * (nk/(nk + 1))^(p/2) * dev^(-(N -
                                                             ng + 1)/2)
    }
  }
  posterior <- dist/drop(dist %*% rep(1, ng))
  nm <- names(object$prior)
  cl <- factor(nm[max.col(posterior)], levels = object$lev)
  dimnames(posterior) <- list(rownames(x), nm)
  list(class = cl, posterior = posterior, x = x[, 1L:dimen,
                                                drop = FALSE])
}
