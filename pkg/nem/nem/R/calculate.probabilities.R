calculate.probabilies <- function(network, theta, actor){
  if (length(theta) == 5) theta <- c(theta, 0, 0, 0)
  # POPULARITY
  popularity <- colSums(network)/nrow(network)
  popularity <- popularity/sum(popularity)

  # FRIENDS
  friends <- common.friends(network, actor = actor)/nrow(network)
  if (sum(friends) != 0) friends <- friends/sum(friends)

  # MUTUALITY
  asyminties <- asymmetric.inties(network, actor = actor)
  asymoutties <- asymmetric.outties(network, actor = actor)

  # HIERARCHICITY
  same.popular <- same.popular.others.than.me(network, actor = actor)

  # RELATIVE CASES
  asyminties.relative <- asyminties/sum(asyminties)
  asyminties.relative[is.nan(asyminties.relative)] <- 0

  asymoutties.relative <- asymoutties/sum(asymoutties)
  asymoutties.relative[is.nan(asymoutties.relative)] <- 0

  same.popular.relative <- same.popular/sum(same.popular)
  same.popular.relative[is.nan(same.popular.relative)] <- 0

  quasi.covariates <- cbind(popularity,
                            friends,
                            asyminties,
                            asymoutties,
                            same.popular,
                            asyminties.relative,
                            asymoutties.relative,
                            same.popular.relative)
  koef <- (quasi.covariates %*% theta)
  koef[is.nan(koef)] <- 0
  probs.koef <- 1/(1 + exp(-koef))
  return(as.vector(probs.koef))
}
