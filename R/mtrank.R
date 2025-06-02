#' Estimate the treatment hierarchy in network meta-analysis using a
#' probabilistic ranking model
#' 
#' @description
#' This function fits the Davidson-Bradley-Terry ranking model and produces a
#' treatment hierarchy based on the method described by
#' Evrenoglou et al. (2024) for network meta-analysis.
#' 
#' @param x An object of class \code{\link{tcc}} or \code{\link{mtrank}}
#'   (print function).
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
#' @param \dots Additional arguments (passed on to
#'   \code{\link[PlackettLuce]{PlackettLuce}} or to \code{\link{prmatrix}}).
#'
#' @details
#' This function fits a Davidson-Bradley-Terry model to the treatment preferences
#  generated from the treatment choice criterion constructed by the
#' \code{\link{tcc}} function. It estimates the ability of
#' each treatment to outperform the other treatments in the network, along with
#' the respective standard errors, using a maximum likelihood approach.
#' The term 'ability to outperform' refers to a latent characteristic that
#' indicates the propensity of each treatment in the network to yield clinically
#' relevant and beneficial treatment effects, in the context of the defined
#' treatment choice criterion, when compared to the rest of the treatments.
#' Consequently, treatments with larger ability estimates are ranked more
#' prominently in the treatment hierarchy.
#' 
#' To retain identifiability, the maximization of the log-likelihood takes place
#' subject to the constrain that the ability estimates sum to 1. Then, the
#' maximum likelihood estimates (MLEs) are calculated iteratively. Note that the
#' final estimates of the ability parameters are not necessarily needed to sum
#' to 1 as after the first iteration of the algorithm the ability estimates are
#' not normalized. However, by normalizing the final ability estimates to sum up
#' to 1 these can be interpreted as "the probability that each treatment is
#' having the highest ability".
#' 
#' Finally, a parameter "v" controlling the prevalence of ties in the network
#' is also estimated. Although the estimated values of this parameter do not
#' have a direct interpretation they are useful for estimating the fitted
#' pairwise probabilities (see \code{\link{fitted.mtrank}}).
#' 
#' @return
#' \itemize{
#' \item A data frame containing the resulting log-ability estimates, their
#'   standard errors and their confidence intervals.
#' \item The estimate of the tie prevalence parameter v, on the log-scale.
#' \item The normalized ability estimates for each treatment. 
#' }
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria,
#' \url{https://arxiv.org/abs/2406.10612}
#' 
#' @examples
#' data("antidepressants")
#' #
#' pw <- pairwise(studlab = studyid, treat = drug_name,
#'   n = ntotal, event = responders,
#'   data = antidepressants, sm = "OR")
#' # Use subset to reduce runtime
#' pw <- subset(pw, studyid < 60)
#' #
#' net <- netmeta(pw, reference.group = "tra")
#' 
#' ranks <- tcc(net, swd = 1.20, small.values = "undesirable")
#' #
#' fit <- mtrank(ranks)
#' 
#' # Print log-ability estimates
#' fit
#' #
#' # Print ability estimates
#' print(fit, backtransf = TRUE)
#' 
#' # Visualize results
#' forest(fit)
#'  
#' @export mtrank

mtrank <- function(x, level = x$level, ...) {
  
  chkclass(x, "tcc")
  #
  if (x$all.ties)
    stop("Ranking cannot be provided as only ties were identified through ",
         "the treatment choice criterion.")
  #
  chklevel(level)
  
  dat <- x$ppdata
  #
  # Fit the model 
  #
  fit <- PlackettLuce(x$preferences,method = "BFGS",...)
  #
  # All estimates and standard errors
  #
  estimates <- summary(fit, ref = NULL)$coef[, 1]
  se_estimates <- summary(fit, ref = NULL)$coef[, 2]
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
  # order the output to range from the treatment with the highest to the
  # treatment with the lowest probability
  #
  dat_prob <- dat_prob[order(-probability), ]
  
  row.names(dat_prob) <- NULL
  
  res <- list(estimates = dat,
              v = v,
              probabilities = dat_prob,
              fit = fit,
              #
              x = x,
              #
              trts = x$trts,
              #
              small.values = x$small.values,
              #
              call = match.call(),
              
              version = packageVersion("mtrank"))
  #
  class(res) <- "mtrank"
  #
  attr(res,"net.obj") <- attributes(x)$net.obj
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
