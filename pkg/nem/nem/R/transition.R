transition <- function(initial, k = 50000, theta, max.friends, lambda, type.sim = "c", q){
  # QUICK HELP
  # k = number of iterations
  # theta = vector of coeficients
  # max.friends = max number of friends
  # lamba = power of theta
  # type.sim = type of simulations
  # q = eqpected density
  statistike.mat <- matrix(NA, nrow = k, ncol = 15)
  for (i in 1:k){
    actor <- sample(nrow(initial), size = 1)
    l.friends <- which(initial[actor,] == 1)
    nonfriends <- which(initial[actor,] == 0)

    if (type.sim == "a"){
      # VZPOSTAVLJANJE POVEZAV
      same.more <- same.popular.others.than.me(initial, actor, error = 5) == 1 | more.popular.others.than.me(initial, actor) == 1
      same.more.ids <- which(same.more)
      less.ids <- setdiff(x = 1:nrow(initial), y = same.more.ids)

      possible.friends<-intersect(nonfriends,same.more.ids)
      unpopular.friends<-intersect(l.friends,less.ids)

      # PREKINJANJE POVEZAV
      probabilities <- calculate.probabilies(initial, theta = theta, actor)

      lenPosFrieds <- length(possible.friends)
      lenUnPopFriends<- length(unpopular.friends)

      if ((length(l.friends) <= max.friends) & ((lenPosFrieds + lenUnPopFriends)>0)){
        addFriend <- runif(1) < lenPosFrieds/(lenPosFrieds + lenUnPopFriends)
        if(addFriend == TRUE) {
          initial[actor, mySample(possible.friends, prob = probabilities[possible.friends]**lambda, 1)] <- 1
        } else {
          initial[actor, mySample(unpopular.friends, prob = (1-probabilities[unpopular.friends])**lambda, 1)] <- 0
        }
      }
      if ((length(l.friends) > max.friends)){
        initial[actor, mySample(l.friends, prob = ((1-probabilities[l.friends])**lambda), 1)] <- 0
      }
    }

    if (type.sim == "b"){
      probabilities <- calculate.probabilies(initial, theta = theta, actor)
      # VZPOSTAVLJANJE POVEZAV
      if ((length(l.friends) < nrow(initial))){
        initial[actor, mySample(nonfriends, prob = ((probabilities[nonfriends])**lambda), 1)] <- 1
      }
      # PREKINJANJE POVEZAV
      if ((length(l.friends) > max.friends)){
        initial[actor, mySample(l.friends, prob = ((1-probabilities[l.friends])**lambda), 1)] <- 0
      }
    }

    if (type.sim == "c"){
      probabilities <- calculate.probabilies(initial, theta = theta, actor)
      if(sample(c(0,1), prob = c(1-q, q), 1) == 1){
        if (length(nonfriends) > 0){
          #initial[actor, nonfriends[which.max(probabilities[nonfriends])]] <- 1
          #initial[sample(nonfriends[is.element(which(probabilities >= quantile(probabilities)[4]), nonfriends)])] <- 1
          initial[actor, sample(which(probabilities >= quantile(probabilities)[4]), 1)] <- 1
        }
      }

      if(sample(c(0,1), prob = c(1-q, q), 1) == 0){
        if (length(l.friends) > 0){
          #initial[actor, l.friends[which.min(probabilities[l.friends])]] <- 0
          #initial[sample(l.friends[is.element(which(probabilities <= quantile(probabilities)[2]), l.friends)])] <- 0
          initial[actor, sample(which(probabilities <= quantile(probabilities)[2]), 1)] <- 0
        }
      }
      statistike.mat[i, ] <- summary(initial ~ triadcensus)
    }
  }
  return(list(initial, statistike.mat))
}
