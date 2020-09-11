minIncBlocks <- function (res, compIM, mustBeConnected = TRUE){
  try(IM <- IM(res))
  try(IM <- IM(res)[, , ], silent = TRUE)

  compIM[compIM == "null"] <- "nul"

  k <- nrow(IM)
  all.perms <- permn(1:k)
  diags <- sapply(1:length(all.perms), function(x) {
    sum((IM[all.perms[[x]], all.perms[[x]]] != compIM) *
          (compIM != "dnc"))
  })
  n.comp <- igraph::components(graph_from_adjacency_matrix(ifelse(IM == "com", yes = 1, no = 0)))$no
  if (mustBeConnected == TRUE) {
    ## moznosti za napake, ce ni povezano omrezje
    #  min(diags) + (n.comp - 1)*k
    # min(diags) + (n.comp - 1)*sum(compIM == "any")
    #  sum(compIM != "any")
    if (n.comp > 1)
      return(list(error = min(diags) + min(diags) + (n.comp - 1) * k, order = all.perms[[which.min(diags)]], n.comp = n.comp))
    if (n.comp == 1)
      return(list(error = min(diags), order = all.perms[[which.min(diags)]], n.comp = n.comp))
  }
  if (mustBeConnected == FALSE) {
    return(list(error = min(diags), order = all.perms[[which.min(diags)]], n.comp = n.comp))
  }
}
