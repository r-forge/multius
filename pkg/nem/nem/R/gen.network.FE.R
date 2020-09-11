gen.network.FE <- function(type = type, randomShare = 0.4, n = 24, size = rep(1/nrow(type), nrow(type)), symmetric = FALSE){
  if (sum(size) != 1) warning("The sum of vector 'size' is not 1!")
  razbitje <- rep(1:length(size), times = size*n)
  network <- matrix(NA, nrow = n, ncol = n)
  for (i in 1:nrow(type)){
    for (j in 1:ncol(type)){
      network[razbitje == i, razbitje == j] <- if (type[i,j] == "com") matrix(1, nrow = size[i]*n, ncol = size[j]*n) else matrix(0,  nrow = size[i]*n, ncol = size[j]*n)
    }
  }
  diag(network) <- -1
  initialNetwork <- network

  if (symmetric == FALSE){
    # koliko povezav je dovoljenih
    stevilo.premaknjenih <- sum(initialNetwork == 0, na.rm = TRUE)*(sum(initialNetwork == 1, na.rm = TRUE)/(n*(n-1)))
    stevilo.premaknjenih.rs <- stevilo.premaknjenih * randomShare
    # ce ni celo stevilo -> zrebaj
    p <- stevilo.premaknjenih.rs-floor(stevilo.premaknjenih.rs)
    stevilo.premaknjenih.rs <- floor(stevilo.premaknjenih.rs) + rbinom(1, 1, p)
    # slucajno izberi povezave
    remove.link <- sample(which(initialNetwork == 1), size = stevilo.premaknjenih.rs)
    make.link <- sample(which(initialNetwork == 0), size = stevilo.premaknjenih.rs)
    # naredi izmenjave
    network[remove.link] <- 0
    network[make.link] <- 1
    diag(network) <- 0
    return(network)
  }

  if (symmetric == TRUE){
    stevilo.premaknjenih <- sum(initialNetwork == 0, na.rm = TRUE)*(sum(initialNetwork == 1, na.rm = TRUE)/(n*(n-1)))
    stevilo.premaknjenih.rs <- (stevilo.premaknjenih * randomShare)/2

    p <- stevilo.premaknjenih.rs-floor(stevilo.premaknjenih.rs)
    stevilo.premaknjenih.rs <- floor(stevilo.premaknjenih.rs) + rbinom(1, 1, p)
    # slucajno izberi povezave
    remove.link <- sample(which(lower.tri(initialNetwork) & initialNetwork == 1), size = stevilo.premaknjenih.rs)
    make.link <- sample(which(lower.tri(initialNetwork) & initialNetwork == 0), size = stevilo.premaknjenih.rs)
    # naredi izmenjave
    network[remove.link] <- 0
    network[make.link] <- 1
    network[upper.tri(network)] = t(network)[upper.tri(network)]
    diag(network) <- 0
    return(network)
  }
}
