iss03 <- function(initial.network,
                       lambda,
                       kappa,
                       newbie,
                       outgoers = NULL,
                       internal.mechanisms,
                       external.mechanisms,
                       external.covariates,
                       theta.im,
                       theta.em,
                       computeTriads = FALSE,
                       loops = FALSE
) {
  if (is.null(outgoers)) outgoers <- rep(0, times = length(newbie) + 1)

  ###################### SAVING NETWORKS
  networks <- list("initial.network" = initial.network)

  ###################### THE DURATION OF LINKS ETC.
  forBeta <- (cumsum(c(nrow(initial.network), newbie)))*kappa*lambda
  steps <- cumsum(forBeta)
  employing <- steps[-length(steps)]

  ###################### WHEN TO LEAVE THE NETWORK
  LeaveTheNetwork <- whenGoOut(forBeta = forBeta, outgoers = outgoers)

  ###################### MATRIX FOR TRIADS COUNTS
  if (computeTriads == TRUE) triads <- matrix(NA, nrow = max(steps), ncol = 15)

  ###################### START THE ALGORITHM ######################
  for (i in 1:max(steps)){
    external.covariates[[1]] <- external.covariates[[1]] + 1/forBeta[which(steps >= i)[1]]
    external.covariates[[2]] <- external.covariates[[2]] + 1/forBeta[which(steps >= i)[1]]
    ###################### EGO
    E <- sample(x = 1:(nrow(initial.network)), size = 1)

    ###################### VALUE-COST
    PEI <- NetworkStatistics(net = ifelse(initial.network > 0, yes = 1, no = 0),
                             actor = E,
                             internal.mechanisms = internal.mechanisms,
                             external.mechanisms = external.mechanisms,
                             external.covariates = external.covariates,
                             theta.im = theta.im,
                             theta.em = theta.em)

    ##################### NORMAIZATION
    PEI <- (PEI - min(PEI))/(max(PEI) - min(PEI))
    PEI[is.nan(PEI)] <- 0

    #################### NAJMANJ IN NAJBOLJ UGODNI
    if (loops == TRUE) {
      D <- which(PEI >= quantile(PEI, probs = 0.75))
    }
    if (loops == FALSE) {
      D <- which(PEI >= quantile(PEI[-E], probs = 0.75))
      D <- D[!(D %in% E)]
    }

    #################### VZPOSTAVLJANJE POVEZAV
    alter <- sample(D, size = 1)
    initial.network[E, alter] <- 1

    ################### DURATION OF LINKS
    beta <- 1/(lambda*nrow(initial.network) + 1)
    initial.network <- initial.network - beta
    initial.network[initial.network < 0] <- 0

    ################### REMOVING UNITS
    if (is.element(i, LeaveTheNetwork)) {
      nRemove <- sum(LeaveTheNetwork==i)
      removeEmployeePack <- removeEmployee(net = initial.network, n = nRemove)
      initial.network <- removeEmployeePack$network
      external.covariates[[1]] <- external.covariates[[2]] <- external.covariates[[1]][removeEmployeePack$remaining.units]
    }

    ################### ADDING UNITS
    if (i %in% employing){
      # save the network
      networks[[as.character(i)]] <- initial.network
      initial.network <- addEmpoyee(net = initial.network, n = newbie[which(i == employing)])
      external.covariates[[1]] <- external.covariates[[2]] <- c(external.covariates[[1]], rep(0, times = newbie[which(i == employing)]))
    }
    # compute triads
    if (computeTriads == TRUE) triads[i,] <- summary(initial.network ~ triadcensus)
  }
  networks[["final.network"]] <- initial.network
  if (computeTriads == FALSE) return(list(networks, "null", employing, "external.covariate" = external.covariates[[1]]))
  if (computeTriads == TRUE) return(list(networks, triads, employing, "external.covariate" = external.covariates[[1]]))
}
