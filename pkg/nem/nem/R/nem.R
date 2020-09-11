nem <- function(initial, formula, theta,  k = 5000, q, b = 0.25){
  # QUICK HELP
  # initial = initial network
  # formula = vector of coefitients
  # k = number of iterations
  # theta = vector of coeficientss
  # q = verjetnost, s katero vzpostavimo povezavo ali (1-b) prekinemo povezavo
  # b = izmed koliko zgornjih izbiramo?
  for (i in 1:k){
    actor <- sample(nrow(initial), size = 1)
    l.friends <- which(initial[actor,] == 1)
    nonfriends <- which(initial[actor,] == 0)

    X <- sapply(X = 1:length(formula), FUN = function(i){formula[[i]](initial, actor = actor)})
    X[is.nan(X)] <- 0
    probabilities <- X %*% theta

    urej  <- cbind(1:nrow(initial), order(probabilities), probabilities)[-actor,]

    if(sample(c(0,1), prob = c(1-q, q), 1) == 1){
      if (length(nonfriends) > 0){
        initial[actor, sample(urej[which(urej[,3] >= quantile(urej[,3], probs = 1-b)), 1], 1)] <- 1
      }
    }

    if(sample(c(0,1), prob = c(1-q, q), 1) == 0){
      if (length(l.friends) > 0){
        initial[actor, sample(urej[which(urej[,3] <= quantile(urej[,3], probs = b)), 1], 1)] <- 0
      }
    }
  }
  return(initial)
}


