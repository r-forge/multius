#' Relocating Links algorithm (RL algorithm)
#'
#' @description It generates random network considering the selected types of triads.
#' @param ideal.net Network with a desired blockmodel without inconsistencies; of class \code{matrix}.
#' @param initial.net Initial network; of class \code{matrix}.
#' @param triads What types of triads has to be considered (allowed \code{allow}, forbidden \code{forb}, all \code{all} or custom \code{cust}). Provide a list of triad types as used in package ergm.
#' @param custom.triads A list with names of a subset of triads to be considered. The same names must be used as in ERGM package. Only if \code{triads = "cust"}.
#' @param k Number of iterations.
#' @return A list contiainig: \code{new.network} which is the generated network (of class \code{matrix}); and \code{CR} which is
#' a vector of CR values (calculated after each iteration).
#' @examples
#' # generate initial and ideal network
#' cohesiveBM <- rbind(c("com", "nul"), c("nul", "com"))
#' ideal <- genNetworkLE(BM = cohesiveBM, LE = 0, size = c(4, 4))
#' random <- genNetworkLE(BM = cohesiveBM, LE = 1, size = c(4, 4))
#' # generate network with the RL algorithm
#' generatedNetwork <- RL(ideal.net = ideal, initial.net = random, triads = "all", k = 10)
#' @references 
#' Cugmas M, Ferligoj A, Žiberna A (2018) Generating global network structures by triad types. PLoS ONE 13(5): e0197514. https://doi.org/10.1371/journal.pone.0197514
#' @author Marjan Cugmas and Aleš Žiberna
#' @import ergm
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
