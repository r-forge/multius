nem.sym <- function(initial, formula, theta,  k = 5000, q){
  # QUICK HELP
  # initial = initial network
  # formula = vector of coefitients
  # k = number of iterations
  # theta = vector of coeficientss
  # q = eqpected density
  for (i in 1:k){
    actor <- sample(nrow(initial), size = 1)
    l.friends <- which(initial[actor,] == 1)
    nonfriends <- which(initial[actor,] == 0)

    X <- matrix(NA, nrow = nrow(initial), ncol = length(formula))
    for (j in 1:length(formula)){
      X[,j] <- formula[[j]](initial, actor = actor)
    }
    X[is.nan(X)] <- 0
    probabilities <- X %*% theta

    if (rbinom(1, 1, prob = q)){
      if (length(nonfriends) > 0){
        cand <- which(probabilities >= quantile(probabilities)[4])
        alter <- sample(cand[!cand%in%actor], 1)
        initial[actor, alter] <- 1
        initial[alter, actor] <- 1
      }
    }

    if (rbinom(1, 1, prob = 1 - q)){
      if (length(l.friends) > 0){
        cand <- which(probabilities <= quantile(probabilities)[2])
        alter <- sample(cand[!cand%in%actor], 1)
        initial[actor, alter] <- 0
        initial[alter, actor] <- 0
      }
    }
  }
  diag(initial) <- 0
  return(initial)
}
