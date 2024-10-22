#' Estimate the treatment hierarchy in network meta-analysis using probabilistic ranking models.
#' 
#' @description
#' This function fits the Bradley-Terry ranking model and produces a treatment hierarchy based
#' on the method described by Evrenoglou et al. for network meta-analysis. 
#' 
#' @param x The output of the function \code{\link{tcc}}.
#' @param reference.group An argument specifying the reference group. If set to NULL (default)
#' the output of the mtrank function will contain the ability estimates of all treatments. If 
#' some treatment is set a the reference group then the output of the mtrank function will contain the 
#' relative abilities of all treatments versus the specified reference treatment.
#'   
#' @details
#' This function is used to fit a Bradley-Terry model to the paired-preference data generated from the treatment choice criterion 
#' constructed by the \code{\link{tcc}} function. The mtrank() function estimates the ability of each treatment in the network and the respective
#' standard errors and confidence intervals using the maximum likelihood approach. To retain identifiability the maximization of the log-likelihood
#' takes place subject to the constrain that the ability estimates sum to 1. Then, based on iterative processes the maximum likelihood estimates (MLEs)
#' are calculated. Note that the final estimates of the ability parameters are not necessarily needed to sum to 1 as after the first iteration of the algorithm
#' the ability estimates are not normalized. However, by normalizing the final ability estimates to sum up to 1 these can be interpreted as "the probability that
#' each treatment is having the highest ability". Finally, a parameter "v" controlling the prevalence of ties in the network is also estimated through
#' the mtrank() function. Although the estimated values of this parameter does not have a direct interpretation it is useful for estimating pairwise 
#' probabilities using the \code{\link{paired_pref}} function. 
#' 
#' If the argument \code{ref} is not NULL then a reference treatment group is specified according to this treatment. Mathematically, this means
#' that the maximization problem is now identifiable subject to the condition that the ability of this treatment is 0. Then the resulting MLEs
#' are the relative abilities of all treatments in the network versus the specified reference treatment group. Note that the estimates of the 
#' parameter "v" and the normalized probabilities do not depend on the definition or not of a reference treatment group. 
#' 
#' @return
#' \itemize{
#' \item A data frame containing the resulting log-ability estimates, their standard errors and their confidence intervals.
#' \item The estimate of the parameter tie prevalence parameter v.
#' \item The normalized ability estimates for each treatment. 
#' }
#' 
#' @references
#' Evrenoglou E, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#' 
#' @examples
#' data(Stowe2010, package = "netmeta")
#' #
#' ranks <- tcc(treat = list(t1,t2,t3), studlab = study, 
#' mean = list(y1,y2,y3), n = list(n1,n2,n3), sd = list(sd1,sd2,sd3), 
#' data = Stowe2010, sm = "MD", small.values = "desirable",mcid = 0.5)
#' #
#' model <- mtrank(ranks)
#' #
#' paired_pref(model,
#'    treat1 = "MAOBI", treat2 = "Dopamine Agonist", type = "better")
#' 
#' @export mtrank


mtrank <- function(x, reference.group = NULL) {
  
  chkclass(x, "tcc")
  #
  if (isTRUE(x$all.ties))
    stop("Ranking cannot be provided as only ties were identified through ",
         "the treatment choice criterion.")
  
  data_wide <- x$data_wide
  #data_wide$comparison <- paste(data_wide$treat1, data_wide$treat2, sep = " vs ")
  #
  data_pref <- x$data_pref
  #
  trts <- unique(c(data_wide$treat1, data_wide$treat2))
  #
  reference.group <- setchar(reference.group, trts)
  
  #
  # Fit the model 
  #
  model <- PlackettLuce(rankings = data_pref)
  #
  # All estimates and standard errors
  #
  estimates <- summary(model, ref = reference.group)$coef[, 1]
  se_estimates <- summary(model, ref = reference.group)$coef[, 2]
  #
  # Isolate the estimate for the parameter 'v'. 
  #
  sel.tie2 <- names(estimates) == "tie2"
  #
  # extract log-abilities and standard errors
  #
  log_ability <- estimates[!sel.tie2]
  se_log_ability <- se_estimates[!sel.tie2]
  #
  # save the results
  #
  dat <- data.frame(treatment = names(log_ability),
                    log_ability = unname(log_ability),
                    se = unname(se_log_ability))
  
  #
  # add 95% confidence intervals 
  #
  logAB.ci <- ci(dat$log_ability, dat$se, level = 0.95)
  #
  dat$lower <- logAB.ci$lower
  dat$upper <- logAB.ci$upper
  
  # get the value of the nuisance parameter; this value is not interpretable. It depends on the proliferation of ties obtained from the TTC. 
  # However, it worth saving it as it can be helpful: (i) for the completeness of the report, 
  # (ii) for using it and calculating pairwise probabilities using equations (7) and (8) from the main manuscript.
  
  v <- estimates[sel.tie2]
  
  
  #
  # calculate the probability that each treatment is the best. 
  #
  probability <- itempar(model, log = FALSE)
  
  dat_prob <- data.frame(treatment = names(probability),
                         probability = as.numeric(probability))
  
  #
  # order the output to range from the treatment with the highest to the treatment with the lowest probability
  #
  dat_prob <- dat_prob[order(-probability), ]
  
  row.names(dat_prob) <- NULL
  
  res <- list(estimates = dat,
              v = v,
              #
              probabilities = dat_prob,
              data = data_pref,
              #
              trts = trts,
              reference.group = reference.group,
              #
              x = x,
              #
              call = match.call(),
              version = packageVersion("mtrank"))
  #
  class(res) <- c("mtrank")
  #
  res
}
