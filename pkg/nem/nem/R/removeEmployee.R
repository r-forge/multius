removeEmployee <- function(net, n){
  # ideja je, da slucajno izberemo enote,
  # ki bodo zapustile podjetje
  # - v primeru, ko enote podjetje zapuscajo posamicno, bi lahko bila ta funkcija poenostavljena
  if ((nrow(net)-n) < 0) stop("The number of outgoers is too high!")
  ostali <- sort(sample(nrow(net), size = nrow(net)-n))
  net <- net[ostali, ostali]
  return(list("network" = net, "remaining.units" = ostali))
}
