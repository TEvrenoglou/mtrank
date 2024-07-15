#' Vizualize ability esimates produced from the function \code{\link{mtrank}}.
#' 
#' @description
#' This function produces a forest plot that vizualizes the ability estimates produced from the output of the R function \code{\link{mtrank}}.
#' 
#' @param x An object of class "mtrank.fit" generated as the output of the \code{\link{mtrank}} function. 
#' @param sorting An argument specifying the criterion for sorting the ability estimates in the forest plot (see Details).
#' @param backtransf An argument specifying the scale of the ability estimates. If FALSE (default) the forest plot vizualizes 
#' the log-ability estimates. If TRUE the scale is transformed to the natural scale of ability estimates. 
#'   
#' @details  
#' The function produces a forest plot and vizualizes the ability estimates obtained from the function \code{\link{mtrank}}. The order of the
#' estimates in the resulting forest plot can be one of the following:
#' \itemize{
#' \item "ability": This is the default option and sorts the estimates in a descending order accoring to the ability estimates.
#' \item "se": Sorts the ability estimates from depending on their uncertainty. The estimats with the lower standard errors appear first.
#' \item "none": Sorts the estmates in a random order.
#' }
#' 
#' @return
#' A forest plot that shows the ability estimates produced from the function \code{\link{mtrank}}.
#' 
#' @references
#' \itemize{
#' \item Theodoros Evrenoglou, Adriani Nikolakopoulou, Guido Schwarzer, Gerta Rücker, Anna Chaimani (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic models and treatment-choice criteria.
#' \emph{https://arxiv.org/abs/2406.10612}
#' }


viz.mtrank.fit <- function(x,sorting="ability",backtransf=FALSE){
  
  if(inherits(x, "mtrank.fit")){
    
    res <- x$estimates
    
    ref <- attributes(x)$ref
    
  }else{
    
    stop("Argument not of 'mtrank.fit' class")
    
  }
  
  if(sorting %!in% c("ability","se","none")){
    
    stop("Unknown ordering pattern. Please specify as either 'ability' or 'se' or 'none'.")
    
  }
  
  if(missing(x)){
    stop("Please specify the argument 'x'.")
  }  
  
  # if(missing(sorting)){
  #   
  #   stop("Please specify the argument order.")
  #   
  # }  
  
  ### order data   
  
  if(sorting=="ability") {
    
    res <- res[order(-res$log_ability),]
    
  } else if(sorting=="se"){
    
    res <- res[order(res$se),]   
    
  } else if(sorting=="none"){
    
    res <- res
  }
  
  
  if(!is.null(ref)){
    
  res <- res[complete.cases(res$se),]
    
  }else{
    res <- res
  }
  
  ### fit a pairwise meta-analysis model
  
  model.rma <- rma(yi=log_ability,sei=se,slab = treatment,data = res)    
  
  
  #### 
  
  if(backtransf==TRUE){
    
    xlab <- "Ability [95% CI]"
    
    
  }else{
    xlab <- "Log-Ability [95% CI]"
  }
  
  
  ### create the forest plot   
  
  if(isTRUE(backtransf)){
    metafor::forest(model.rma,
                    atransf=exp,
                    psize=1,
                    addfit=F,
                    mlab="",
                    xlab=xlab
    )  
    
  }else{
    metafor::forest(model.rma,
                    psize=1,
                    addfit=F,
                    mlab="",
                    xlab=xlab
    ) 
  }
  
  
  
}