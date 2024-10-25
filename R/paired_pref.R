#' Calculate pairwise probabilities for \code{\link{mtrank}} object
#' 
#' @description
#' This function uses the estimates of ability and tie prevalence parameters
#' from a \code{\link{mtrank}} object and calculates pairwise probabilities
#' about the preference or the tie between two treatments based on equations (7)
#' and (8) in Evrenoglou et al. (2024).
#  
#' @param x An object of class \code{\link{mtrank}}.
#' @param treat1 The first treatment considered in the treatment comparison.
#' @param treat2 The second treatment considered in the treatment comparison.
#' @param type A character vector specifying the probability of interest.
#'   Either "better", "tie", "worse", or "all" (can be abbreviated).
#' @param digits Minimal number of significant digits for proportions,
#'   see \code{print.default}.
#' @param \dots Additional arguments (passed on to \code{\link{prmatrix}}).
#'
#' @details  
#' Pairwise probabilities between any two treatments in the network can be
#' calculated using the ability estimates obtained from \code{\link{mtrank}}
#' and equations (7) and (8) in Evrenoglou et al. (2024). The probabilities
#' are calculated in the direction \code{treat1} vs \code{treat2}. The available
#' probability types are
#' \itemize{
#' \item "better": probability that \code{treat1} is better than \code{treat2},
#' \item "tie": probability that \code{treat1} is equal to \code{treat2},
#' \item "worse": probability that \code{treat1} is worse than \code{treat2},
#' \item "all": all three probabilities.
#' }
#' 
#' Please note that all the arguments of this function are mandatory.
#' 
#' @return
#' The probability (or probabilities) of interest for the comparison
#' \code{treat1} vs \code{treat2} based on the argument \code{type}.
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#' 
#' @examples
#' data(antidepressants)
#' #
#' ranks <- tcc(treat = drug_name, studlab = studyid,
#'   event = responders, n = ntotal, data = antidepressants,
#'   mcid = 1.25, sm = "OR", small.values = "undesirable")
#' #
#' fit <- mtrank(ranks)
#' #
#' paired_pref(fit, type = c("better", "worse"),
#'   treat1 = "bupropion", treat2 = "escitalopram")
#' #
#' paired_pref(fit, type = c("better", "worse"),
#'   treat1 = "escitalopram", treat2 = "bupropion")
#' #
#' paired_pref(fit, type = "all",
#'   treat1 = c("bupropion", "escitalopram"),
#'   treat2 = c("escitalopram", "bupropion"))
#'
#' @export paired_pref

paired_pref <- function(x, treat1, treat2, type) {
  
  chkclass(x, "mtrank")
  
  treat1 <- setchar(treat1, x$x$trts)
  treat2 <- setchar(treat2, x$x$trts)
  #
  type <- setchar(type, c("better", "tie", "worse", "all"))
  type <- unique(type)
  if ("all" %in% type)
    type <- "all"
  #
  if (length(treat1) == 1 & length(treat2) > 1)
    treat1 <- rep(treat1, length(treat2))
  else if (length(treat2) == 1 & length(treat1) > 1)
    treat2 <- rep(treat2, length(treat1))
  #
  if (length(treat1) != length(treat2))
    stop("Arguments 'treat1' and 'treat2' must be of same length.",
         call. = FALSE)
  
  # To calculate the paired preferences we need the results of mtrank()
  # without a reference group (argument reference.group = NULL) so that we have
  # the ability estimates for each treatment
  #
  if (!is.null(x$reference.group))
    fit <- mtrank(x$x, reference.group = NULL)
  else
    fit <- x
  
  # Extract the ability estimate for 'treat1' and 'treat2' on the natural scale  
  #
  psi1 <- psi2 <- rep(NA, length(treat1))
  #
  for (i in seq_along(treat1)) {
    psi1[i] <-
      exp(fit$estimates[fit$estimates$treatment == treat1[i], ]$log_ability)
    #
    psi2[i] <-
      exp(fit$estimates[fit$estimates$treatment == treat2[i], ]$log_ability)
  }
  #
  # extract 'v' parameter estimate
  #
  v <- fit$v
  #
  # Use equation (7) to get probability that 'treat1' is better than 'treat2'
  #
  p_better <- psi1 / (psi1 + psi2 + v * sqrt(psi1 * psi2))
  #
  # Use equation (8) to get probability that 'treat1' is tied to 'treat2'
  #
  p_tie <- (v * sqrt(psi1 * psi2)) / (psi1 + psi2 + v * sqrt(psi1 * psi2))
  #
  # Probability that 'treat1' is worse than 'treat2'
  # based on Pr(treat1 > treat2) + Pr(treat1 = treat2) + Pr(treat1 < treat2) = 1
  #
  p_worse <- 1 - p_better - p_tie
  #
  res <- data.frame(treat1, treat2, p_better, p_tie, p_worse)
  class(res) <- c("paired_pref", class(res))
  attr(res, "type") <- type
  #
  res
}


#' @rdname paired_pref
#' @method print paired_pref
#' @export

print.paired_pref <- function(x, type = attr(x, "type"), digits = 4, ...) {
  
  chkclass(x, "paired_pref")
  #
  type <- setchar(type, c("better", "tie", "worse", "all"))
  type <- unique(type)
  if ("all" %in% type)
    type <- "all"
  #
  if (length(type) == 1 && type == "all") {
    print_better <- TRUE
    print_tie <- TRUE
    print_worse <- TRUE
  }
  else {
    print_better <- print_tie <- print_worse <- FALSE
    #
    if ("better" %in% type)
      print_better <- TRUE
    if ("tie" %in% type)
      print_tie <- TRUE
    if ("worse" %in% type)
      print_worse <- TRUE
  }
  
  if (nrow(x) == 1) {
    # Report results according to type of interest (for single comparison)
    #
    treat1 <- x$treat1
    treat2 <- x$treat2
    #
    p_better <- x$p_better
    p_tie <- x$p_tie
    p_worse <- x$p_worse
    #
    txt1 <- paste0("The probability that ",
                   treat1," is better than ", treat2, " is equal to: ",
                   round(p_better, digits = digits),
                   "\n")
    #
    txt2 <- paste0("The probability that ",
                   treat1, " is tied to ", treat2, " is equal to: ",
                   round(p_tie, digits = digits),
                   "\n")
    #
    txt3 <- paste0("The probability that ",
                   treat1," is worse than ", treat2, " is equal to: ",
                   round(p_worse, digits = digits),
                   "\n")
    #
    txt <- ""
    if (print_better)
      txt <- txt1
    if (print_tie)
      txt <- paste0(txt, txt2)
    if (print_worse)
      txt <- paste0(txt, txt3)
    #
    cat(txt)
  }
  else {
    treats <- x %>% select(treat1, treat2)
    res_p <- x %>% select(p_better, p_tie, p_worse) %>% round(digits = digits)
    #
    if (!print_better)
      res_p %<>% select(-p_better)
    if (!print_tie)
      res_p %<>% select(-p_tie)
    if (!print_worse)
      res_p %<>% select(-p_worse)
    #
    prmatrix(cbind(treats, res_p), quote = FALSE, right = TRUE,
             rowlab = rep("", nrow(res_p)), ...)
  }
  
  invisible(NULL)
}
