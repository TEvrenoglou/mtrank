#' Estimate the treatment hierarchy in network meta-analysis using a
#' probabilistic ranking model
#' 
#' @description
#' This function fits the Bradley-Terry ranking model and produces a treatment
#' hierarchy based on the method described by Evrenoglou et al. (2024) for
#' network meta-analysis.
#' 
#' @param x An object of class \code{\link{tcc}} or \code{\link{mtrank}}
#'   (print function).
#' @param reference.group An argument specifying the reference group. If set to
#'   NULL (default), ability estimates of all treatments will be calculated.
#'   If some treatment is set as the reference group, relative abilities of all
#'   treatments versus the specified reference treatment will be calculated.
#' @param level The level used to calculate confidence intervals for ability
#'   estimates.
#' @param backtransf A logical argument specifying whether to show log-ability
#'   estimates (\code{FALSE}, default) or on the natural scale (\code{TRUE}).
#' @param digits Minimal number of significant digits for ability estimates,
#'   see \code{print.default}.
#' @param sorting An argument specifying the criterion to sort the ability
#' estimates in the printout (see Details).
#' @param digits.prop Minimal number of significant digits for proportions,
#'   see \code{print.default}.
#' @param \dots Additional arguments (passed on to \code{\link{prmatrix}}).
#'
#' @details
#' This function is used to fit a Bradley-Terry model to the paired-preference
#' data generated from the treatment choice criterion constructed by the
#' \code{\link{tcc}} function. This function estimates the ability of
#' each treatment in the network and the respective standard errors and
#' confidence intervals using the maximum likelihood approach. To retain
#' identifiability, the maximization of the log-likelihood takes place subject
#' to the constrain that the ability estimates sum to 1. Then, the maximum
#' likelihood estimates (MLEs) are calculated iteratively.
#' Note that the final estimates of the ability parameters are not necessarily
#' needed to sum to 1 as after the first iteration of the algorithm the ability
#' estimates are not normalized. However, by normalizing the final ability
#' estimates to sum up to 1 these can be interpreted as "the probability that
#' each treatment is having the highest ability".
#' 
#' Finally, a parameter "v" controlling the prevalence of ties in the network
#' is also estimated. Although the estimated values of this parameter do
#' not have a direct interpretation they are useful for estimating pairwise 
#' probabilities (see \code{\link{paired_pref}}).
#' 
#' If argument \code{reference.group} is not NULL, a reference treatment
#' group is specified. Mathematically, this means that the maximization problem
#' is now identifiable, subject to the condition that the ability of this
#' treatment is 0. Then, the resulting MLEs are the relative abilities of all
#' treatments in the network versus the specified reference treatment group.
#' Note that the estimates of the parameter "v" and the normalized probabilities
#' do not depend on the value for argument \code{reference.group}.
#' 
#' @return
#' \itemize{
#' \item A data frame containing the resulting log-ability estimates, their
#'   standard errors and their confidence intervals.
#' \item The estimate of the tie prevalence parameter v.
#' \item The normalized ability estimates for each treatment. 
#' }
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#' 
#' @examples
#' data(antidepressants)
#' 
#' ranks <- tcc(treat = drug_name, studlab = studyid,
#'   event = responders, n = ntotal, data = antidepressants,
#'   mcid = 1.25, sm = "OR", small.values = "undesirable")
#' #
#' fit1 <- mtrank(ranks)
#' #
#' # Print log-ability estimates
#' fit1
#' #
#' # Print ability estimates
#' print(fit1, backtransf = TRUE)
#' # Visualize results 
#'  forest(fit1)
#' 
#' # Repeat using a 'pairwise' object
#' pw <- pairwise(treat = drug_name, studlab = studyid,
#'   event = responders, n = ntotal, data = antidepressants,
#'   sm = "OR")
#'   
#' ranks2 <- tcc(pw, mcid = 1.25, small.values = "undesirable")
#' #
#' fit2 <- mtrank(ranks2) 
#'   
#' # Print log-ability estimates
#' fit2
#' # Print ability estimates
#' print(fit2, backtransf = TRUE)
#' # Visualize results 
#'  forest(fit2)
#'  
#' @export mtrank

mtrank <- function(x, reference.group = NULL, level = x$level) {
  
  chkclass(x, "tcc")
  #
  if (x$all.ties)
    stop("Ranking cannot be provided as only ties were identified through ",
         "the treatment choice criterion.")
  #
  chklevel(level)
  
  dat <- x$ppdata
  #dat$comparison <- paste(dat$treat1, dat$treat2, sep = " vs ")
  #
  reference.group <- setchar(reference.group, x$trts)
  #
  # Fit the model 
  #
  fit <- PlackettLuce(x$grouped.preferences)
  #
  # All estimates and standard errors
  #
  estimates <- summary(fit, ref = reference.group)$coef[, 1]
  se_estimates <- summary(fit, ref = reference.group)$coef[, 2]
  #
  # Isolate the estimate for the parameter 'v'. 
  #
  sel.tie2 <- names(estimates) == "tie2"
  #
  # Extract log-abilities and standard errors
  #
  log_ability <- estimates[!sel.tie2]
  se_log_ability <- se_estimates[!sel.tie2]
  #
  # Save the results
  #
  dat <- data.frame(treatment = names(log_ability),
                    log_ability = unname(log_ability),
                    se = unname(se_log_ability))
  
  #
  # Add 95% confidence intervals 
  #
  logAB.ci <- ci(dat$log_ability, dat$se, level = level)
  #
  dat$lower <- logAB.ci$lower
  dat$upper <- logAB.ci$upper
  
  #
  # Get the value of the nuisance parameter; this value is not interpretable.
  # It depends on the proliferation of ties obtained from the TTC. 
  # However, it worth saving it as it can be helpful:
  # (i) for the completeness of the report, 
  # (ii) for using it and calculating pairwise probabilities using equations
  #      (7) and (8) from the main manuscript.
  #
  
  v <- estimates[sel.tie2]
  
  
  #
  # Calculate the probability that each treatment is the best. 
  #
  probability <- itempar(fit, log = FALSE)
  #
  dat_prob <- data.frame(treatment = names(probability),
                         probability = as.numeric(probability))
  
  #
  # order the output to range from the treatment with the highest to the treatment with the lowest probability
  #
  dat_prob <- dat_prob[order(-probability), ]
  
  row.names(dat_prob) <- NULL
  
  res <- list(estimates = dat,
              v = v,
              probabilities = dat_prob,
              fit = fit,
              reference.group = reference.group,
              #
              x = x,
              #
              call = match.call(),
              version = packageVersion("mtrank"))
  #
  class(res) <- "mtrank"
  #
  res
}


#' @rdname mtrank
#' @method print mtrank
#' @export

print.mtrank <- function(x,
                         sorting = "ability",
                         backtransf = FALSE,
                         digits = gs("digits"),
                         digits.prop = gs("digits.prop"),
                         ...) {
  chkclass(x, "mtrank")
  #
  sorting <- setchar(sorting, c("ability", "se", "none"))
  chklogical(backtransf)
  chknumeric(digits, min = 0, length = 1)
  chknumeric(digits.prop, min = 0, length = 1)
  
  dat <- x$estimates
  dat$se <- NULL
  #
  props <- x$probabilities
  props$probability <- round(props$probability, digits.prop)
  #
  dat <- merge(dat, props, by = "treatment")
  #
  if (backtransf) {
    dat$log_ability <- exp(dat$log_ability)
    dat$lower <- exp(dat$lower)
    dat$upper <- exp(dat$upper)
  }
  #
  if (sorting == "ability")
    o <- order(-dat$log_ability)
  else if (sorting == "se")
    o <- order(dat$se)
  else if (sorting == "none")
    o <- seq_len(nrow(dat))
  #
  dat <- dat[o, , drop = FALSE]
  #
  dat$log_ability <- round(dat$log_ability, digits)
  dat$lower <- round(dat$lower, digits)
  dat$upper <- round(dat$upper, digits)
  #
  if (backtransf)
    names(dat)[names(dat) == "log_ability"] <- "ability"
  #
  prmatrix(dat, quote = FALSE, right = TRUE,
           rowlab = rep("", nrow(dat)), ...)
  #
  invisible(NULL)
}
