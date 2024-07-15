#' Estimate the treatment hierarchy in network meta-analysis using probabilistic ranking models.
#' 
#' @description
#' This function fits the Bradley-Terry ranking model and produces a treatment hiearachy based
#' on the method described by Evrenoglou et al. for network meta-analysis. 
#' 
#' @param data The output of the function TCC.
#' @param ref An argument specifying the reference group. If set to NULL (default)
#' the output of the mtrank function will contain the ability estimates of all treatments. If 
#' some treatment is set a the reference group then the output of the mtrank function will contain the 
#' relative abilities of all treatments versus the specified reference treatment.
#'   
#'   
#' @details
#' This function is used to fit a Bradley-Terry model to the paired-preference data generated from the treatment choice criterion 
#' constructed by the \code{\link{TCC}} function. The mtrank() function estimates the ability of each treatment in the network and the respective
#' standard errors and confidence intervals using the maximum likelihood approach. To retain identifiability the maximization of the log-likelihood
#' takes place subject to the constrain that the ability estimates sum to 1. Then, based on iterative processes the maximum likelihood estimates (MLEs)
#' are calculated. Note that the final estimates of the ability parameters are not necessarily needed to sum to 1 as after the first iteration of the algorithm
#' the ability estimates are not normalized. However, by normalizing the final ability estimates to sum up to 1 these can be interpreted as "the probability that
#' each treatment is having the highest ability". Finally, a parameter "v" controlling the prevalence of ties in the network is also estimated through
#' the mtrank() function. Although the estimated values of this parameter does not have a direct interpretation it is useful for estimating pairwise 
#' probabilities using the \code{\link(paired.pref)} function. 
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
#' \itemize{
#' \item Theodoros Evrenoglou, Adriani Nikolakopoulou, Guido Schwarzer, Gerta Rücker, Anna Chaimani (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic models and treatment-choice criteria.
#' \emph{https://arxiv.org/abs/2406.10612}
#' }


mtrank <- function(data, ref=NULL){
  
  if(inherits(data, "mtrank")){
    
    data_wide <- attributes(data)$data_pairwise
    
    all.ties <- attributes(data)$all.ties
    
    data_wide$comp <- paste(data_wide$treat1,data_wide$treat2,sep = " vs ")
    
    all_treats <- c(unique(data_wide$treat1),unique(data_wide$treat2))
    
    all_treats <- unique(all_treats)
    
  }else{
    stop("Please, check your 'data' argument as currently it is not of 'mtrank' class.")  
  }
  
  if(!(is.null(ref)) && (ref %!in% all_treats)){
    
    stop("Wrong reference treatment. The reference should be one of the available treatment options in the network: ",paste(all_treats,collapse = ", "))
    
  }
  
  if(isTRUE(all.ties)){
    
    stop("Ranking cannot be provided as only ties were identified through the treatment choice criterion.")
    
  }
  
  preference.data <- data$preference.data
  
  #### Fit the model 
  
  model <- PlackettLuce::PlackettLuce(rankings = preference.data)
  
  ### All estimates and standard errors
  
  estimates <- summary(model,ref=ref)$coef[,1]
  
  se_estimates <- summary(model,ref=ref)$coef[,2]
  
  ### Isolate the estimate for the parameter 'v'. 
  
  E <- which(names(estimates)=="tie2")
  
  ## extract log-abilities and standard errors
  
  log_ability <- estimates[-E]
  
  se_log_ability <- se_estimates[-E]
  
  ## save the results
  res <- cbind.data.frame("treatment"=names(log_ability),
                          "log_ability" = unname(round(log_ability,digits = 4)),
                          "se" = unname(round(se_log_ability,digits = 4))
  )
  
  
  ## add 95% confidence intervals 
  
  lb.ci <- round(res$log_ability - 1.96*res$se,digits = 4)
  
  ub.ci <- round(res$log_ability + 1.96*res$se,digits = 4)
  
  res$lb.ci <- lb.ci
  
  res$ub.ci <- ub.ci
  
  ## get the value of the nuisance parameter; this value is not interpretable. It depends on the proliferation of ties obtained from the TTC. 
  ## However, it worth saving it as it can be helpful: (i) for the completness of the report, 
  # (ii) for using it and calculating pairwise probabilities using equations (7) and (8) from the main manuscript.
  
  v <- round(estimates[E],digits = 4)
  
  names(v) <- "v"
  
  
  ### calculate the probability that each treatment is the best. 
  
  probability <- PlackettLuce::itempar(model,log=F)
  
  res_prob <- cbind.data.frame("treatment" = names(probability),
                               "probability" = round(as.numeric(probability),digits = 4)
  )
  

                   
  ## order the output to range from the treatment with the highest to the treatment with the lowest probability
  res_prob <- res_prob[order(-probability),]
  
  row.names(res_prob) <- NULL
  
  final_res <- list("estimates" = res,
                    "v"=v,
                    "probabilities" = res_prob
  )
  
  
  class(final_res) <- c("mtrank.fit","mtrank")
  
  attr(final_res,"preference.data") <- preference.data
  
  attr(final_res,"all_treats") <- all_treats
  
  attr(final_res,"ref") <- ref
  
  
  return(final_res)
}

