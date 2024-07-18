#' Calculate pairwise probabilities using the output of the \code{\link{mtrank}} function.
#' 
#' @description
#' This function uses the estimates of the ability and tie prevalence parameters obtained by the \code{\link{mtrank}} function
#' and calculates pairwise probabilities about the preference or the tie between two treatments based on Equations 7 and 8 
#' from the manuscript by Evrenoglou et al.
#
#' @param x An object of class "mtrank".
#' @param treat1 The first treatment considered in the treatment comparison.
#' @param treat2 The second treatment considered in the treatment comparison.
#' @param type A character string specifying the probability of interest.
#'   Either "better", "tie", "worse", or "all" (can be abbreviated).
#'
#' @details  
#' Pairwise probabilities between any two treatments in the network can be calculated using the ability estimates
#' obtained from the \code{\link{mtrank}} function and the Equations 7 and 8 from Evrenoglou et al. The probabilities
#' are calculated in the direction \code{treat1} vs \code{treat2}. The available probability types are
#' \itemize{
#' \item "better": Calculates the probability that \code{treat1} is better than \code{treat2}.
#' \item "tie": Calculates the probability that \code{treat1} is equal to \code{treat2}.
#' \item "worse": Calculates the probability that \code{treat1} is worse than \code{treat2}.
#' \item "all": Calculates all the previous probabilities simultaneously.
#' }
#' 
#' Please note that all the arguments of this function are mandatory.
#' 
#' @return
#' The probability (or probabilities) of interest for the comparison \code{treat1} vs \code{treat2} based on the argument \code{type}.
#' 
#' @references
#' Evrenoglou E, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#' 
#' @export paired_pref



paired_pref <- function(x, treat1, treat2, type) {
  
  chkclass(x, "mtrank")
    
  treat1 <- setchar(treat1, x$trts)
  treat2 <- setchar(treat2, x$trts)
  #
  type <- setchar(type, c("better", "tie", "worse", "all"))
  
  # To calculate the paired preferences we need access to the results of
  # mtrank() with reference.group = NULL so that we have the ability estimates
  # for each treatment  
  
  if (!is.null(x$reference.group))
    model <- mtrank(x$x, reference.group = NULL)
  else
    model <- x
  
  # extract the ability estimate for 'treat1' and 'treat2' on a natural scale  
  
  psi.t1 <-
    exp(model$estimates[model$estimates$treatment == treat1, ]$log_ability)
  
  psi.t2 <-
    exp(model$estimates[model$estimates$treatment == treat2, ]$log_ability)
  #
  # extract 'v' parameter estimate
  #
  v <- model$v
  #
  # get probability that 'treat1' is better than 'treat2' using Equation 7 from the main manuscript
  #
  pr_t1.t2 <- psi.t1 / (psi.t1 + psi.t2 + v * sqrt(psi.t1 * psi.t2))
  #
  # get probability that 'treat1' is tied to 'treat2' using Equation 8 from the main manuscript
  #
  pr_t1.t2.tie <-
    (v * sqrt(psi.t1 * psi.t2)) /
    (psi.t1 + psi.t2 + v * sqrt(psi.t1 * psi.t2))
  #
  # get probability that 'treat1' is worse than 'treat2' using that Pr(treat1>treat2)+Pr(treat1=treat2)+Pr(treat1<treat2)=1
  #
  pr_t2.t1 <- 1 - pr_t1.t2 - pr_t1.t2.tie
  #
  # report the results according to the type of interest
  #
  txt1 <- paste0("The probability that ", treat1," is better than ",
                 treat2, " is equal to: ", round(pr_t1.t2, digits = 4))
  #
  txt2 <- paste0("The probability that ", treat1," is tied to ",
                 treat2, " is equal to: ", round(pr_t1.t2.tie, digits = 4))
  #
  txt3 <- paste0("The probability that ", treat1," is worse than ",
                 treat2, " is equal to: ", round(pr_t2.t1,digits = 4))
  #
  if (type == "better")
    txt <- txt1
  else if (type == "tie")
    txt <- txt2 
  else if (type == "worse")
    txt <- txt3
  else if (type == "all")
    txt <- list(txt1, txt2, txt3) 
  #
  txt
}
