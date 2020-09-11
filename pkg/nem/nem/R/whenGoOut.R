whenGoOut <- function(forBeta, outgoers, oneBYone = FALSE){
  # ideja je, da izracunamo iteracije med vsakim valom, ko se dodajo novi
  # nato pa vzorcimo iteracije ob katerih odstranjujemo stare
  beta_start <- c(1, cumsum(forBeta)[-length(forBeta)] + 1)
  beta_stop <- cumsum(forBeta)
  timeToLeave <- NULL
  for (i in 1:(length(beta_start))){
    timeToLeave <- c(timeToLeave, sample(beta_start[i]:beta_stop[i], replace = ifelse(oneBYone, yes = FALSE, no = TRUE), size = outgoers[i]))
  }
  return(timeToLeave)
}
