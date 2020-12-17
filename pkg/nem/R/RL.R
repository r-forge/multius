#' Relocating Links algorithm (RL algorithm)
#'
#' @description The function generate random network considering the selected types of triads.
#' @param ideal.net Network with an ideal (desired) blockmodel; of class \code{matrix}.
#' @param initial.net Initial network; of class \code{matrix}.
#' @param triads Which types of triads has to be considered (allowed \code{allow}, forbidden \code{forb}, all \code{all} or custom \code{cust}).
#' @param custom.triads Additional terms to be considered. Only if \code{terms = "cust"}.
#' @param k Number of iterations.
#' @return A list contiainig: \code{new.network} which is the generated network; and \code{CR} which is
#' a vector of CR values (a value is calculated after each iteration).
#' @examples
#' # generate initial and ideal network
#' cohesiveBM <- rbind(c("com", "nul"), c("nul", "com"))
#' ideal <- gen.network.LE(BM = cohesiveBM, LE = 0, n = 12, size = rep(0.5, 2))
#' random <- gen.network.LE(BM = cohesiveBM, LE = 1, n = 12, size = rep(0.5, 2))
#' # generate network with the RL algorithm
#' generatedNetwork <- RL(ideal.net = ideal, initial.net = random, triads = "all", k = 1000)
#' # evaluate the obtained network
#' res <- blockmodeling::optRandomParC(M = generatedNetwork[[1]], k = 2, approaches = "bin", blocks = c("null", "com"), rep = 1000, nCores = 1)
#' @source Cugmas M, Ferligoj A, ?iberna A (2018) Generating global network structures by triad types. PLoS ONE 13(5): e0197514. https://doi.org/10.1371/journal.pone.0197514
#' @author Marjan Cugmas
#' @export

RL <- function(ideal.net, initial.net, triads = "forb", k = 100, custom.triads = NULL){
  n <- nrow(ideal.net)
  teoreticna.porazdelitev <- ergm::summary_formula(ideal.net ~ triadcensus)
  if (triads=="forb") allowed.terms <- names(teoreticna.porazdelitev)[teoreticna.porazdelitev == 0]
  if (triads=="allow") allowed.terms <- names(teoreticna.porazdelitev)[teoreticna.porazdelitev != 0]
  if (triads=="all") allowed.terms <- names(teoreticna.porazdelitev)
  if (triads=="cust") allowed.terms <- names(teoreticna.porazdelitev)[custom.triads]
  change.ratio.vec <- NULL
  new.network <- initial.net
  for (i in 1:k){
    # izberi eno ne-povezavo
    pov <- sample(which(initial.net == 0)[is.element(which(initial.net == 0), which(diag(1, nrow = n) == 1)) == FALSE], size = 1)
    # izberi eno povezavo
    npov <- sample(which(initial.net == 1), size = 1)
    # zamenjaj edge
    new.network[pov] <- 1
    new.network[npov] <- 0
    # izracunaj statistike
    s.rand <- summary(initial.net ~ triadcensus)
    s.new <- summary(new.network ~ triadcensus)

    nova.teoreticna <- sum((s.new[allowed.terms] - teoreticna.porazdelitev[allowed.terms])**2)
    stara.teoreticna <- sum((s.rand[allowed.terms] - teoreticna.porazdelitev[allowed.terms])**2)

    change.ratio.vec[i] <- change.ratio <- nova.teoreticna/stara.teoreticna

    if (is.nan(change.ratio) == FALSE) {
      if (change.ratio < 1) initial.net <- new.network
      else new.network <- initial.net
    } else new.network <- initial.net
  }
  return(list("new.network" = new.network, "CR" = change.ratio.vec))
}
