## Most functions containd in this file were collected and somethimes adapted by Aleš Žiberna from the following sources:

## File BoxMTest.R (location unknown), Written by Andy Liaw (2004), slight clean-up and fix with corrected documentation provided by Ranjan Maitra (2012)

## Functions in package MASS

## R Data Analysis Examples: Canonical Correlation Analysis,  UCLA: Statistical Consulting Group.
## from http://www.ats.ucla.edu/stat/r/dae/canonical.htm (accessed Decembar 27, 2013).


######################################################################
## Adapted from package MASS by Aleš Žiberna (2016)

# lda<- function (x, ...){
#     UseMethod("lda")
# }
#
lda.default <- function (x, grouping, prior = proportions, tol = 1e-04, method = c("moment", "mle", "mve", "t"), CV = FALSE, nu = 5, usePriorBetweenGroups=TRUE, ...) {
    if (is.null(dim(x)))
        stop("'x' is not a matrix")
    x <- as.matrix(x)
    if (any(!is.finite(x)))
        stop("infinite, NA or NaN values in 'x'")
    n <- nrow(x)
    p <- ncol(x)
    if (n != length(grouping))
        stop("nrow(x) and length(grouping) are different")
    g <- as.factor(grouping)
    lev <- lev1 <- levels(g)
    counts <- as.vector(table(g))
    if (!missing(prior)) {
        if (any(prior < 0) || round(sum(prior), 5) != 1)
            stop("invalid 'prior'")
        if (length(prior) != nlevels(g))
            stop("'prior' is of incorrect length")
        prior <- prior[counts > 0L]
    }
    if (any(counts == 0L)) {
        empty <- lev[counts == 0L]
        warning(sprintf(ngettext(length(empty), "group %s is empty",
                                 "groups %s are empty"), paste(empty, collapse = " ")),
                domain = NA)
        lev1 <- lev[counts > 0L]
        g <- factor(g, levels = lev1)
        counts <- as.vector(table(g))
    }
    proportions <- counts/n
    ng <- length(proportions)
    names(proportions)<-names(prior) <- names(counts) <- lev1


    if(usePriorBetweenGroups){
        betweenGroupsWeights<-prior
    } else betweenGroupsWeights<-proportions
    method <- match.arg(method)
    if (CV && !(method == "moment" || method == "mle"))
        stop(gettext("cannot use leave-one-out CV with method %s",
                     sQuote(method)), domain = NA)
    group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
    f1 <- sqrt(diag(var(x - group.means[g, ])))
    if (any(f1 < tol)) {
        const <- format((1L:p)[f1 < tol])
        stop(sprintf(ngettext(length(const), "variable %s appears to be constant within groups",
                              "variables %s appear to be constant within groups"),
                     paste(const, collapse = " ")), domain = NA)
    }
    scaling <- diag(1/f1, , p)
    if (method == "mve") {
        cov <- n/(n - ng) * cov.rob((x - group.means[g, ]) %*%
                                        scaling)$cov
        sX <- svd(cov, nu = 0L)
        rank <- sum(sX$d > tol^2)
        if (rank == 0L)
            stop("rank = 0: variables are numerically constant")
        if (rank < p)
            warning("variables are collinear")
        scaling <- scaling %*% sX$v[, 1L:rank] %*% diag(sqrt(1/sX$d[1L:rank]),
                                                        , rank)
    }
    else if (method == "t") {
        if (nu <= 2)
            stop("'nu' must exceed 2")
        w <- rep(1, n)
        repeat {
            w0 <- w
            X <- x - group.means[g, ]
            sX <- svd(sqrt((1 + p/nu) * w/n) * X, nu = 0L)
            X <- X %*% sX$v %*% diag(1/sX$d, , p)
            w <- 1/(1 + drop(X^2 %*% rep(1, p))/nu)
            print(summary(w))
            group.means <- tapply(w * x, list(rep(g, p), col(x)),
                                  sum)/rep.int(tapply(w, g, sum), p)
            if (all(abs(w - w0) < 0.01))
                break
        }
        X <- sqrt(nu/(nu - 2) * (1 + p/nu)/n * w) * (x - group.means[g,
                                                                     ]) %*% scaling
        X.s <- svd(X, nu = 0L)
        rank <- sum(X.s$d > tol)
        if (rank == 0L)
            stop("rank = 0: variables are numerically constant")
        if (rank < p)
            warning("variables are collinear")
        scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank],
                                                         , rank)
    }
    else {
        fac <- if (method == "moment")
            1/(n - ng)
        else 1/n
        X <- sqrt(fac) * (x - group.means[g, ]) %*% scaling
        X.s <- svd(X, nu = 0L)
        rank <- sum(X.s$d > tol)
        if (rank == 0L)
            stop("rank = 0: variables are numerically constant")
        if (rank < p)
            warning("variables are collinear")
        scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank],
                                                         , rank)
    }
    if (CV) {
        x <- x %*% scaling
        dm <- group.means %*% scaling
        K <- if (method == "moment")
            ng
        else 0L
        dist <- matrix(0, n, ng)
        for (i in 1L:ng) {
            dev <- x - matrix(dm[i, ], n, rank, byrow = TRUE)
            dist[, i] <- rowSums(dev^2)
        }
        ind <- cbind(1L:n, g)
        nc <- counts[g]
        cc <- nc/((nc - 1) * (n - K))
        dist2 <- dist
        for (i in 1L:ng) {
            dev <- x - matrix(dm[i, ], n, rank, byrow = TRUE)
            dev2 <- x - dm[g, ]
            tmp <- rowSums(dev * dev2)
            dist[, i] <- (n - 1L - K)/(n - K) * (dist2[, i] +
                                                     cc * tmp^2/(1 - cc * dist2[ind]))
        }
        dist[ind] <- dist2[ind] * (n - 1L - K)/(n - K) * (nc/(nc -
                                                                  1))^2/(1 - cc * dist2[ind])
        dist <- 0.5 * dist - matrix(log(prior), n, ng, byrow = TRUE)
        dist <- exp(-(dist - min(dist, na.rm = TRUE)))
        cl <- factor(lev1[max.col(dist)], levels = lev)
        posterior <- dist/drop(dist %*% rep(1, length(prior)))
        dimnames(posterior) <- list(rownames(x), lev1)
        return(list(class = cl, posterior = posterior))
    }

    xbar <- colSums(betweenGroupsWeights %*% group.means)
    fac <- if (method == "mle")
        1/ng
    else 1/(ng - 1)
    X <- sqrt((n * betweenGroupsWeights) * fac) * scale(group.means, center = xbar, scale = FALSE) %*% scaling
    X.s <- svd(X, nu = 0L)
    rank <- sum(X.s$d > tol * X.s$d[1L])
    if (rank == 0L)
        stop("group means are numerically identical")
    scaling <- scaling %*% X.s$v[, 1L:rank]
    if (is.null(dimnames(x)))
        dimnames(scaling) <- list(NULL, paste("LD", 1L:rank,
                                              sep = ""))
    else {
        dimnames(scaling) <- list(colnames(x), paste("LD", 1L:rank,
                                                     sep = ""))
        dimnames(group.means)[[2L]] <- colnames(x)
    }
    cl <- match.call()
    cl[[1L]] <- as.name("lda")
    ngs <- n * betweenGroupsWeights
    vg<-by(data = x,INDICES = g,function(x)apply(x,2,stats::var))
    v<-0
    for(i in names(ngs)){
        v<-v+vg[[i]]*(ngs[i]-1)
    }
    v<-v/(sum(ngs-1))
    sds<-sqrt(v)
    standCoefWithin<-diag(sds)%*%scaling

    standCoefTotal<-diag(apply(x,2,sd))%*%scaling

    rownames(standCoefWithin)<-rownames(standCoefTotal)<-rownames(scaling)
    structure(list(prior = prior, counts = counts, means = group.means,
                   scaling = scaling, standCoefWithin=standCoefWithin, standCoefTotal=standCoefTotal, lev = lev, svd = X.s$d[1L:rank], N = n, betweenGroupsWeights=betweenGroupsWeights, call = cl), class = "lda")
}
#
# predict.lda<- function (object, newdata, prior = object$prior, dimen, method = c("plug-in", "predictive", "debiased"), betweenGroupsWeights=object$betweenGroupsWeights, ...) {
#     if (!inherits(object, "lda"))
#         stop("object not of class \"lda\"")
#     if (!is.null(Terms <- object$terms)) {
#         Terms <- delete.response(Terms)
#         if (missing(newdata))
#             newdata <- model.frame(object)
#         else {
#             newdata <- model.frame(Terms, newdata, na.action = na.pass,
#                                    xlev = object$xlevels)
#             if (!is.null(cl <- attr(Terms, "dataClasses")))
#                 .checkMFClasses(cl, newdata)
#         }
#         x <- model.matrix(Terms, newdata, contrasts = object$contrasts)
#         xint <- match("(Intercept)", colnames(x), nomatch = 0L)
#         if (xint > 0L)
#             x <- x[, -xint, drop = FALSE]
#     }
#     else {
#         if (missing(newdata)) {
#             if (!is.null(sub <- object$call$subset))
#                 newdata <- eval.parent(parse(text = paste(deparse(object$call$x,
#                                                                   backtick = TRUE), "[", deparse(sub, backtick = TRUE),
#                                                           ",]")))
#             else newdata <- eval.parent(object$call$x)
#             if (!is.null(nas <- object$call$na.action))
#                 newdata <- eval(call(nas, newdata))
#         }
#         if (is.null(dim(newdata)))
#             dim(newdata) <- c(1L, length(newdata))
#         x <- as.matrix(newdata)
#     }
#     if (ncol(x) != ncol(object$means))
#         stop("wrong number of variables")
#     if (length(colnames(x)) > 0L && any(colnames(x) != dimnames(object$means)[[2L]]))
#         warning("variable names in 'newdata' do not match those in 'object'")
#     ng <- length(object$prior)
#     if (!missing(prior)) {
#         if (any(prior < 0) || round(sum(prior), 5) != 1)
#             stop("invalid 'prior'")
#         if (length(prior) != ng)
#             stop("'prior' is of incorrect length")
#     }
#
#     if(is.null(betweenGroupsWeights))betweenGroupsWeights<-prior
#     means <- colSums(betweenGroupsWeights * object$means)
#     scaling <- object$scaling
#     x <- scale(x, center = means, scale = FALSE) %*% scaling
#     dm <- scale(object$means, center = means, scale = FALSE) %*%
#         scaling
#     method <- match.arg(method)
#     dimen <- if (missing(dimen)) {
#         length(object$svd)
#     }else min(dimen, length(object$svd))
#     N <- object$N
#     if (method == "plug-in") {
#         dm <- dm[, 1L:dimen, drop = FALSE]
#         dist <- matrix(0.5 * rowSums(dm^2) - log(prior), nrow(x),
#                        length(prior), byrow = TRUE) - x[, 1L:dimen, drop = FALSE] %*%
#             t(dm)
#         dist <- exp(-(dist - apply(dist, 1L, min, na.rm = TRUE)))
#     }
#     else if (method == "debiased") {
#         dm <- dm[, 1L:dimen, drop = FALSE]
#         dist <- matrix(0.5 * rowSums(dm^2), nrow(x), ng, byrow = TRUE) -
#             x[, 1L:dimen, drop = FALSE] %*% t(dm)
#         dist <- (N - ng - dimen - 1)/(N - ng) * dist - matrix(log(prior) -
#                                                                   dimen/object$counts, nrow(x), ng, byrow = TRUE)
#         dist <- exp(-(dist - apply(dist, 1L, min, na.rm = TRUE)))
#     }
#     else {
#         dist <- matrix(0, nrow = nrow(x), ncol = ng)
#         p <- ncol(object$means)
#         X <- x * sqrt(N/(N - ng))
#         for (i in 1L:ng) {
#             nk <- object$counts[i]
#             dev <- scale(X, center = dm[i, ], scale = FALSE)
#             dev <- 1 + rowSums(dev^2) * nk/(N * (nk + 1))
#             dist[, i] <- prior[i] * (nk/(nk + 1))^(p/2) * dev^(-(N -
#                                                                      ng + 1)/2)
#         }
#     }
#     posterior <- dist/drop(dist %*% rep(1, ng))
#     nm <- names(object$prior)
#     cl <- factor(nm[max.col(posterior)], levels = object$lev)
#     dimnames(posterior) <- list(rownames(x), nm)
#     list(class = cl, posterior = posterior, x = x[, 1L:dimen,
#                                                   drop = FALSE])
# }
#

######################################################################
### addapted from the source below by Aleš Žiberna (2013)
### Source:
## R Data Analysis Examples: Canonical Correlation Analysis,  UCLA: Statistical Consulting Group.
## from http://www.ats.ucla.edu/stat/r/dae/canonical.htm (accessed Decembar 27, 2013).

# tests of canonical correlations
# testCCbase<-function(cor, n, p, q){
#     # cor - cannonical correlations
#     # n - number of units
#     # p - number of variables (first group)
#     # q - number of variables (second group)
#     ev <- (1 - cor^2)
#     k <- min(p, q)
#     m <- n - 3/2 - (p + q)/2
#     w <- rev(cumprod(rev(ev)))
#
#     # initialize
#     d1 <- d2 <- f <- vector("numeric", k)
#
#     for (i in 1:k) {
#         s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
#         si <- 1/s
#         d1[i] <- p * q
#         d2[i] <- m * s - p * q/2 + 1
#         r <- (1 - w[i]^si)/w[i]^si
#         f[i] <- r * d2[i]/d1[i]
#         p <- p - 1
#         q <- q - 1
#     }
#
#     pv <- pf(f, d1, d2, lower.tail = FALSE)
#     eig<-cor^2/(1 - cor^2)
#     eigModel<-cbind(Eigenvalues=eig,"%"=eig/sum(eig)*100,"Cum %"=cumsum(eig/sum(eig))*100,"Cor"=cor,"Sq. Cor"=cor^2)
#
#     dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
#     rownames(dmat)<-paste(1:k,"to",k)
#     return(list(sigTest=dmat,eigModel=eigModel))
# }

# testCC<-function(cancor,n){
#     testCCbase(cor=cancor$cor, n, p=dim(cancor$xcoef)[1], q=dim(cancor$ycoef)[1])
# }


# cancorPlus<-function(x, y, xcenter = TRUE, ycenter = TRUE){
#     if(!require(MASS)) stop("MASS Package is required but not installed!")
#     cca<-cancor(x=x,y=y,xcenter=xcenter,ycenter=ycenter)
#     n<-dim(x)[1]
#     p<-dim(cca$xcoef)[1]
#     q<-dim(cca$ycoef)[1]
#     tmp<-testCCbase(cor=cca$cor, n=n, p=p, q=q)
#     res<-c(cca,tmp)
#     class(res)<-class(cca)
#     return(res)
# }

# ldaPlus<-function(x,grouping,pred=FALSE, CV=FALSE,usePriorBetweenGroups=TRUE,...){
#     call<-match.call()
#     if(!require(MASS)) stop("MASS Package is required but not installed!")
#     LDA<-lda(x=x,grouping=grouping, CV=FALSE,usePriorBetweenGroups=usePriorBetweenGroups,...)
#     LDA$call<-call
#     n<-dim(x)[1]
#     p<-dim(x)[2]
#     dummy<-model.matrix(~ grouping)[,-1,drop=FALSE]
#     colnames(dummy)<-sub(pattern="^grouping",replacement="",x=colnames(dummy))
#     q<-dim(dummy)[2]
#
#     if(!is.null(list(...)$prior) & usePriorBetweenGroups) warning("The specified prior is not taken into account when computing eigenvalues and all statistics based on them (everything in components 'eigModel' and 'sigTest' of the returned value.")
#     cca<-cancor(x=x,y=dummy)
#     tmp<-testCCbase(cor=cca$cor, n=n, p=p, q=q)
#
#     res<-c(LDA,tmp)
#     class(res)<-class(LDA)
#
#     if(pred){
#       res$pred<-predict(LDA)
#
#       res$centroids<-aggregate(res$pred$x,by = list(grouping),FUN=mean)
#       rownames(res$centroids)<-res$centroids[,1]
#       res$centroids<-res$centroids[,-1]
#       tmp<-by(cbind(x,res$pred$x),INDICES=grouping,FUN=cov,simplify=TRUE)
#       tmp<-simplify2array(tmp)
#       d1<-dim(tmp)[1]
#
#       tmp<-sweep(tmp,MARGIN = 3,STATS = res$counts-1,FUN = "*")
#       covw<-apply(tmp,MARGIN = 1:2,FUN = sum)/(res$N - length(res$counts))
#       res$corr<-cov2cor(covw)[-((p+1):d1),((p+1):d1),drop=FALSE]
#
#
#       tab<-table(orig=grouping,pred=res$pred$class)
#       res$class<-list(
#         orgTab=addmargins(tab,margin = 1:2),
#         # % of correct predictions by groups
#         perTab =  addmargins(prop.table(tab,1)*100,margin=2),
#         # % of correct predictions
#         corPer=sum(diag(prop.table(tab)))*100
#       )
#     }
#
#     if(CV){
#       cv<-lda(x=x,grouping=grouping, CV=TRUE,usePriorBetweenGroups=usePriorBetweenGroups,...)
#       tab<-table(orig=grouping,pred=cv$class)
#       res$classCV<-list(
#         orgTab=addmargins(tab,margin = 1:2),
#         # % of correct predictions by groups
#         perTab =  addmargins(prop.table(tab,1)*100,margin=2),
#         # % of correct predictions
#         corPer=sum(diag(prop.table(tab)))*100
#       )
#     }
#     return(res)
# }



# ccPlus<-function(X,Y){
#   if(!require(CCA)) stop("CCA Package is required but not installed!")
#   cca<-cc(X=X,Y=Y)
#   n<-dim(X)[1]
#   p<-dim(cca$xcoef)[1]
#   q<-dim(cca$ycoef)[1]
#   tmp<-testCCbase(cor=cca$cor, n=n, p=p, q=q)
#   res<-c(cca,tmp)
#   class(res)<-class(cca)
#   return(res)
# }


######################################################################
## mapLda written by Aleš Žiberna (2016)

# mapLda<- function (object, xlim=c(-2,2),ylim=c(-2,2), npoints=101, prior = object$prior, dimen=2, method = c("plug-in", "predictive", "debiased"), col=NULL,...)
# {
#     if (!inherits(object, "lda"))
#         stop("object not of class \"lda\"")
#     ng <- length(object$prior)
#     if (!missing(prior)) {
#         if (any(prior < 0) || round(sum(prior), 5) != 1)
#             stop("invalid 'prior'")
#         if (length(prior) != ng)
#             stop("'prior' is of incorrect length")
#     }
#     prop<-object$counts/sum(object$counts)
#     means <- colSums(prop * object$means)
#     scaling <- object$scaling
#     dm <- scale(object$means, center = means, scale = FALSE) %*%
#         scaling
#     LD1<-seq(from=xlim[1],to=xlim[2],length.out = npoints)
#     LD2<-seq(from=ylim[1],to=ylim[2],length.out = npoints)
#     x<-cbind(rep(LD1,times=npoints),rep(LD2,each=npoints))
#     dm <- dm[, 1L:dimen, drop = FALSE]
#     dist <- matrix(0.5 * rowSums(dm^2) - log(prior), nrow(x),
#                    length(prior), byrow = TRUE) - x[, 1L:dimen, drop = FALSE] %*% t(dm)
#     dist <- exp(-(dist - apply(dist, 1L, min, na.rm = TRUE)))
#     posterior <- dist/drop(dist %*% rep(1, ng))
#     nm <- names(object$prior)
#     cl <- factor(nm[max.col(posterior)], levels = object$lev)
#     list(class = cl, posterior = posterior, x = x)
#     if(is.null(col))col<-1:ng
#     image(LD1,LD2,matrix(as.numeric(cl), nrow=npoints), col=col)
# }


