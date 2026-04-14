#' Ordinal c-index (ORC) with pairwise results tables
#'
#' Computes the ordinal c-index (ORC) as the mean of pairwise AUCs across all
#' outcome category pairs, closely matching the SPSS implementation discussed
#' earlier. For each pair of categories `(i, j)`, the function restricts the
#' data to those two categories and computes the AUC using the pair-specific
#' conditional probability
#' \deqn{P(Y = j \mid Y \in \{i,j\}, x) = p_j / (p_i + p_j).}
#'
#' A weighted version is also reported using weights \eqn{n_i n_j}. This
#' weighted version is a practical extension and not the original ORC definition.
#'
#' @param outcome A vector containing the observed ordinal outcome. May be a
#'   numeric/integer vector or an ordered/unordered factor. Its unique values
#'   define the outcome categories.
#' @param probs A numeric matrix or data frame of predicted probabilities with
#'   one row per observation and one column per outcome category.
#' @param categories Optional vector giving the category values in the same order
#'   as the columns of `probs`. If `NULL`, the sorted unique values of `outcome`
#'   are used.
#' @param na_rm If `TRUE`, observations with missing values in `outcome` or
#'   `probs` are removed listwise.
#'
#' @return A list with class `"orc_result"` containing:
#'   \describe{
#'     \item{basic_info}{Data frame with outcome variable summary information.}
#'     \item{pairwise}{Data frame with one row per category pair and columns
#'       `category_i`, `category_j`, `n_i`, `n_j`, `auc`, `weight`.}
#'     \item{summary}{Data frame with unweighted and weighted ORC.}
#'     \item{orc_unweighted}{Numeric scalar, mean of pairwise AUCs.}
#'     \item{orc_weighted}{Numeric scalar, weighted mean of pairwise AUCs using
#'       `n_i * n_j`.}
#'     \item{categories}{Category vector actually used.}
#'     \item{n_valid}{Number of valid cases used.}
#'   }
#'
#' @details
#' This implementation is closest to the probability-based pairwise ORC
#' definition:
#' for each pair `(i, j)`, it uses a pair-specific score derived from the model
#' predictions. For standard proportional-odds models, this produces the same
#' ranking-based ORC as score-based implementations using a single linear
#' predictor or expected-value score.
#'
#' @references
#' Van Calster, B., Van Belle, V., Vergouwe, Y., & Steyerberg, E. W. (2012).
#' Discrimination ability of prediction models for ordinal outcomes:
#' relationships between existing measures and a new measure.
#' \emph{Statistics in Medicine}, 31(21), 2317-2331.
#'
#' @examples
#' # Example with artificial probabilities:
#' # outcome <- c(1, 1, 2, 2, 3, 3)
#' # probs <- rbind(
#' #   c(.60, .30, .10),
#' #   c(.50, .35, .15),
#' #   c(.25, .55, .20),
#' #   c(.20, .50, .30),
#' #   c(.10, .35, .55),
#' #   c(.05, .25, .70)
#' # )
#' # res <- orc_table(outcome, probs)
#' # res$pairwise
#' # res$summary
#'
#' @export
orc_table <- function(outcome,
                      probs,
                      categories = NULL,
                      na_rm = TRUE) {
  outcomeName<-deparse(substitute(outcome))[1L]
  probs <- as.data.frame(probs)
  
  if (length(outcome) != nrow(probs)) {
    stop("Length of 'outcome' must equal number of rows in 'probs'.")
  }
  
  if (na_rm) {
    ok <- !is.na(outcome) & stats::complete.cases(probs)
    outcome <- outcome[ok]
    probs <- probs[ok, , drop = FALSE]
  }
  
  if (length(outcome) == 0L) {
    stop("No valid cases remain after removing missing values.")
  }
  
  if (is.null(categories)) {
    if (is.factor(outcome)) {
      categories <- levels(factor(outcome))
    } else {
      categories <- sort(unique(outcome))
    }
  }
  
  categories_chr <- as.character(categories)
  outcome_chr <- as.character(outcome)
  
  if (length(categories_chr) != ncol(probs)) {
    stop("Number of categories must match number of columns in 'probs'.")
  }
  
  auc_binary_rank <- function(labels, scores) {
    n1 <- sum(labels == 1L)
    n0 <- sum(labels == 0L)
    if (n1 == 0L || n0 == 0L) return(NA_real_)
    ranks <- rank(scores, ties.method = "average")
    sum_ranks_pos <- sum(ranks[labels == 1L])
    (sum_ranks_pos - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  
  pair_rows <- vector("list", length(categories_chr) * (length(categories_chr) - 1L) / 2L)
  idx_row <- 1L
  
  aucs <- numeric(0)
  weights <- numeric(0)
  
  for (i in seq_len(length(categories_chr) - 1L)) {
    for (j in (i + 1L):length(categories_chr)) {
      ci <- categories_chr[i]
      cj <- categories_chr[j]
      
      idx <- outcome_chr %in% c(ci, cj)
      yy <- outcome_chr[idx]
      pp <- probs[idx, c(i, j), drop = FALSE]
      
      denom <- pp[[1]] + pp[[2]]
      valid_pair <- denom > 0 & !is.na(denom)
      
      yy <- yy[valid_pair]
      pp <- pp[valid_pair, , drop = FALSE]
      denom <- denom[valid_pair]
      
      ni <- sum(yy == ci)
      nj <- sum(yy == cj)
      
      if (length(yy) == 0L || ni == 0L || nj == 0L) {
        auc_ij <- NA_real_
      } else {
        score <- pp[[2]] / denom
        resp <- ifelse(yy == cj, 1L, 0L)
        auc_ij <- auc_binary_rank(resp, score)
      }
      
      wt <- ni * nj
      
      pair_rows[[idx_row]] <- data.frame(
        category_i = ci,
        category_j = cj,
        n_i = as.integer(ni),
        n_j = as.integer(nj),
        auc = auc_ij,
        weight = as.integer(wt),
        stringsAsFactors = FALSE
      )
      idx_row <- idx_row + 1L
      
      if (!is.na(auc_ij)) {
        aucs <- c(aucs, auc_ij)
        weights <- c(weights, wt)
      }
    }
  }
  
  pairwise <- do.call(rbind, pair_rows)
  
  orc_unweighted <- mean(aucs)
  orc_weighted <- stats::weighted.mean(aucs, weights)
  
  basic_info <- data.frame(
    Description = c("Outcome variable", "Categories", "Number of valid cases"),
    Value = c(
      outcomeName,
      paste0("[", paste(categories_chr, collapse = ", "), "]"),
      as.character(length(outcome_chr))
    ),
    stringsAsFactors = FALSE
  )
  
  summary <- data.frame(
    Index = c(
      "ORC (unweighted mean of pairwise AUCs)",
      "ORC (weighted by n_i * n_j)"
    ),
    Estimate =c(orc_unweighted, orc_weighted),
    stringsAsFactors = FALSE
  )
  
  
  out <- list(
    basic_info = basic_info,
    pairwise = pairwise,
    summary = summary,
    orc_unweighted = orc_unweighted,
    orc_weighted = orc_weighted,
    categories = categories,
    n_valid = length(outcome_chr)
  )
  class(out) <- "orc_result"
  out
}


#' Hand-Till multiclass AUC with pairwise results tables
#'
#' Computes the Hand-Till multiclass AUC for nominal outcomes by averaging
#' symmetric pairwise AUCs across all category pairs. For each pair `(i, j)`,
#' the function computes:
#' \deqn{A(i|j)}
#' using the predicted probability for class `i`, and
#' \deqn{A(j|i)}
#' using the predicted probability for class `j`. The pairwise Hand-Till AUC is
#' \deqn{(A(i|j) + A(j|i))/2.}
#'
#' A weighted version is also reported using weights \eqn{n_i n_j}. This
#' weighted version is a practical extension and not the original Hand-Till
#' definition.
#'
#' @param outcome A vector containing the observed nominal outcome. May be a
#'   factor, character, numeric, or integer vector.
#' @param probs A numeric matrix or data frame of predicted probabilities with
#'   one row per observation and one column per outcome category.
#' @param categories Optional vector giving the category values in the same order
#'   as the columns of `probs`. If `NULL`, the factor levels of `outcome` are
#'   used when `outcome` is a factor; otherwise sorted unique values are used.
#' @param na_rm If `TRUE`, observations with missing values in `outcome` or
#'   `probs` are removed listwise.
#'
#' @return A list with class `"ht_auc_result"` containing:
#'   \describe{
#'     \item{basic_info}{Data frame with outcome variable summary information.}
#'     \item{pairwise}{Data frame with one row per category pair and columns
#'       `category_i`, `category_j`, `n_i`, `n_j`, `A_i_given_j`,
#'       `A_j_given_i`, `pairwise_auc`, `weight`.}
#'     \item{summary}{Data frame with unweighted and weighted Hand-Till AUC.}
#'     \item{ht_unweighted}{Numeric scalar, mean of pairwise Hand-Till AUCs.}
#'     \item{ht_weighted}{Numeric scalar, weighted mean using `n_i * n_j`.}
#'     \item{categories}{Category vector actually used.}
#'     \item{n_valid}{Number of valid cases used.}
#'   }
#'
#' @references
#' Hand, D. J., & Till, R. J. (2001).
#' A simple generalisation of the area under the ROC curve for multiple class
#' classification problems.
#' \emph{Machine Learning}, 45(2), 171-186.
#'
#' @examples
#' # Example with artificial probabilities:
#' # outcome <- c("A", "A", "B", "B", "C", "C")
#' # probs <- rbind(
#' #   c(.65, .20, .15),
#' #   c(.55, .25, .20),
#' #   c(.20, .60, .20),
#' #   c(.15, .55, .30),
#' #   c(.10, .25, .65),
#' #   c(.10, .20, .70)
#' # )
#' # colnames(probs) <- c("A", "B", "C")
#' # res <- ht_auc_table(outcome, probs)
#' # res$pairwise
#' # res$summary
#'
#' @export
ht_auc_table <- function(outcome,
                         probs,
                         categories = NULL,
                         na_rm = TRUE) {
  outcomeName<-deparse(substitute(outcome))[1L]
  probs <- as.data.frame(probs)
  
  if (length(outcome) != nrow(probs)) {
    stop("Length of 'outcome' must equal number of rows in 'probs'.")
  }
  
  if (na_rm) {
    ok <- !is.na(outcome) & stats::complete.cases(probs)
    outcome <- outcome[ok]
    probs <- probs[ok, , drop = FALSE]
  }
  
  if (length(outcome) == 0L) {
    stop("No valid cases remain after removing missing values.")
  }
  
  if (is.null(categories)) {
    if (is.factor(outcome)) {
      categories <- levels(factor(outcome))
    } else {
      categories <- sort(unique(outcome))
    }
  }
  
  categories_chr <- as.character(categories)
  outcome_chr <- as.character(outcome)
  
  if (length(categories_chr) != ncol(probs)) {
    stop("Number of categories must match number of columns in 'probs'.")
  }
  
  auc_binary_rank <- function(labels, scores) {
    n1 <- sum(labels == 1L)
    n0 <- sum(labels == 0L)
    if (n1 == 0L || n0 == 0L) return(NA_real_)
    ranks <- rank(scores, ties.method = "average")
    sum_ranks_pos <- sum(ranks[labels == 1L])
    (sum_ranks_pos - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  
  pair_rows <- vector("list", length(categories_chr) * (length(categories_chr) - 1L) / 2L)
  idx_row <- 1L
  
  pair_aucs <- numeric(0)
  weights <- numeric(0)
  
  for (i in seq_len(length(categories_chr) - 1L)) {
    for (j in (i + 1L):length(categories_chr)) {
      ci <- categories_chr[i]
      cj <- categories_chr[j]
      
      idx <- outcome_chr %in% c(ci, cj)
      yy <- outcome_chr[idx]
      pp <- probs[idx, c(i, j), drop = FALSE]
      
      ni <- sum(yy == ci)
      nj <- sum(yy == cj)
      
      if (length(yy) == 0L || ni == 0L || nj == 0L) {
        a_i_given_j <- NA_real_
        a_j_given_i <- NA_real_
        pair_auc <- NA_real_
      } else {
        a_i_given_j <- auc_binary_rank(ifelse(yy == ci, 1L, 0L), pp[[1]])
        a_j_given_i <- auc_binary_rank(ifelse(yy == cj, 1L, 0L), pp[[2]])
        pair_auc <- mean(c(a_i_given_j, a_j_given_i))
      }
      
      wt <- ni * nj
      
      pair_rows[[idx_row]] <- data.frame(
        category_i = ci,
        category_j = cj,
        n_i = as.integer(ni),
        n_j = as.integer(nj),
        A_i_given_j = a_i_given_j,
        A_j_given_i = a_j_given_i,
        pairwise_auc = pair_auc,
        weight = as.integer(wt),
        stringsAsFactors = FALSE
      )
      idx_row <- idx_row + 1L
      
      if (!is.na(pair_auc)) {
        pair_aucs <- c(pair_aucs, pair_auc)
        weights <- c(weights, wt)
      }
    }
  }
  
  pairwise <- do.call(rbind, pair_rows)
  
  ht_unweighted <- mean(pair_aucs)
  ht_weighted <- stats::weighted.mean(pair_aucs, weights)
  
  basic_info <- data.frame(
    Description = c("Outcome variable", "Categories", "Number of valid cases"),
    Value = c(
      outcomeName,
      paste0("[", paste(categories_chr, collapse = ", "), "]"),
      as.character(length(outcome_chr))
    ),
    stringsAsFactors = FALSE
  )
  
  summary <- data.frame(
    Index = c(
      "Hand-Till multiclass AUC (unweighted mean of pairwise AUCs)",
      "Hand-Till multiclass AUC (weighted by n_i * n_j)"
    ),
    Estimate = c(ht_unweighted, ht_weighted),
    stringsAsFactors = FALSE
  )
  

  out <- list(
    basic_info = basic_info,
    pairwise = pairwise,
    summary = summary,
    ht_unweighted = ht_unweighted,
    ht_weighted = ht_weighted,
    categories = categories,
    n_valid = length(outcome_chr)
  )
  class(out) <- "ht_auc_result"
  out
}


#' Print method for ORC results
#'
#' @param x Object returned by [orc_table()].
#' @param digits Number of digits used in rounded summary output. NULL (default) uses R defaults.
#' @param ... Ignored.
#' @export
print.orc_result <- function(x, digits = NULL, ...) {
  cat("\nORC: basic information\n")
  print(x$basic_info, row.names = FALSE, digits=digits)
  cat("\nORC: pairwise category comparisons\n")
  print(x$pairwise, row.names = FALSE, digits=digits)
  cat("\nORC: summary indices\n")
  print(x$summary, row.names = FALSE, digits=digits)
  invisible(x)
}


#' Print method for Hand-Till AUC results
#'
#' @param x Object returned by [ht_auc_table()].
#' @param digits Number of digits used in rounded summary output. NULL (default) uses R defaults.
#' @param ... Ignored.
#' @export
print.ht_auc_result <- function(x, digits = NULL, ...) {
  cat("\nMulticlass AUC: basic information\n")
  print(x$basic_info, row.names = FALSE, digits=digits)
  cat("\nMulticlass AUC: pairwise category comparisons\n")
  print(x$pairwise, row.names = FALSE, digits=digits)
  cat("\nMulticlass AUC: summary indices\n")
  print(x$summary, row.names = FALSE, digits=digits)
  invisible(x)
}