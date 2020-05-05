testCCbase <- function (cor, n, p, q){
  ev <- (1 - cor^2)
  k <- min(p, q)
  m <- n - 3/2 - (p + q)/2
  w <- rev(cumprod(rev(ev)))
  d1 <- d2 <- f <- vector("numeric", k)
  for (i in 1:k) {
    s <- ifelse(p^2 + q^2==5, 1,sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5)))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
  }
  pv <- pf(f, d1, d2, lower.tail = FALSE)
  eig <- cor^2/(1 - cor^2)
  eigModel <- cbind(Eigenvalues = eig, `%` = eig/sum(eig) * 
                      100, `Cum %` = cumsum(eig/sum(eig)) * 100, Cor = cor, 
                    `Sq. Cor` = cor^2)
  dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
  rownames(dmat) <- paste(1:k, "to", k)
  return(list(sigTest = dmat, eigModel = eigModel))
}


