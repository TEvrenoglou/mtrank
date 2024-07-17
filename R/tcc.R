#' Transform meta-analysis data from long or wide arm-based format into
#' paired preference format
#' 
#' @description
#' This function transforms data that are given in wide or long
#' arm-based format (e.g. input format for WinBUGS or JAGS) to a
#' paired-preference format that is needed as input to R function
#' \code{\link{mtrank}}. The function can transform data with binary and
#' continuous arm-based to preference-based format.
#' 
#' @param treat A list or vector with treatment information for
#'   individual treatment arms (see Details).
#' @param event A list or vector with information on number of events
#'   for individual treatment arms (see Details).
#' @param n A list or vector with information on number of
#'   observations for individual treatment arms (see Details).
#' @param mean A list vector with estimated means for individual
#'   treatment arms (see Details).
#' @param sd A list or vector with information on the standard
#'   deviation for individual treatment arms (see Details).
#' @param data A data frame containing the study information.
#' @param studlab A vector with study labels.
#' @param mcid A numeric vector specifying the minimal clinically important value (see Details). 
#' @param l.roe A numeric value specifying the lower limit of the range of equivalence (see Details).
#' @param u.roe A numeric value specifying the upper limit of the range of equivalence (see Details).
#' @param small.values A character string specifying whether small 
#' treatment effects indicate a beneficial (\code{"desirable"}) or
#' harmful (\code{"undesirable"}) effect.
#' @param relax A logical optional argument. If TRUE it 'relaxes' the tcc 
#' to only consider the bounds of ROE when specifying 'wins' and ties. 
#' The default FALSE uses the criterion described by Evrenoglou et al. and considers
#' also the statistical significance on top of the ROE bounds (see Details).
#' @param sm The effect measure of interest (see Details). 
#' @param \dots Additional arguments passed-through to the functions             
#'   
#'   
#' @details
#' R function \code{\link{mtrank}} expects data in a
#' \bold{paired-preference format}, where for each study-specific pairwise comparison in the network
#' a treatment preference or a treatment tie is indicated. For example, for the study-specific comparison
#' between treatments \emph{A} and \emph{B} the potential outcomes are: (i) \emph{A}>\emph{B}
#' (ii) \emph{B}>\emph{A} or (iii) \emph{A}=\emph{B}. 
#' 
#' The data transformation takes place based on 
#' the study-specific treatment effects and the treatment choice criterion. To calculate the study-specific
#' treatment effect estimates and standard errors the tcc function uses the \code{\link{pairwise}} function which is available from the
#' R package netmeta. This ensures that data given in either 'long' or 'wide' arm-based format
#' will be suitably used to calculate the study-specific treatment effect estimates and standard errors while
#' ensuring that a network of multi-arm studies gets and equivalent representation as a network of two-arm studies.
#' 
#' The tcc function includes treatment choice criteria based on the method by Evrenoglou et al.. This approach uses the range of equivalence (ROE) which through the \code{tcc} function can be defined in two different ways.
#'
#' \itemize{
#' \item by specifying the argument \code{mcid}. Then the limits of the ROE will be defined based on the values (i) \code{mcid}, \code{1/mcid} for ratio measures
#' and (ii) \code{mcid} and \code{-mcid} for difference measures.
#' \item by specifying the arguments \code{l.roe} and {u.roe}. These arguments allow the users to define their own limits of the ROE, given the restriction
#' that the lower limit will always be smaller than the upper limit.
#'
#' }
#' 
#' Note that when the argument \code{mcid} is specified, the arguments \code{l.roe} and \code{u.roe} are deprecated. 
#' Either only the \code{mcid} or both of the \code{l.roe} and \code{u.roe} should be specified mandatorily specified for the proper
#' definition of the ROE.
#' 
#' After setting the ROE the tcc function uses it to specify whether each of the study-specific treatment effects refers to a 
#' treatment preference or a tie. The argument \code{relax} controls the amount of conservatism of the treatment choice criterion
#' If set as FALSE (default) then the treatment choice criterion is equivalent to the one described by Evrenoglou et al., 2024. 
#' In this case the study-specific treatment effect need to be both statistically and clinically significant to indicate a treatment preference. If set to TRUE
#' the criterion is relaxed and the study-specific treatment effects need to be only clinically significant to indicate a treatment preference. 
#' 
#' The tcc function can transform data with binary and continuous outcomes.
#''Depending on the outcome, the following arguments are mandatory:
#' \itemize{
#' \item treat, event, n (for binary outcomes);
#' \item treat, n, mean, sd (for continuous outcomes);
#' }
#' 
#' Finally, the argument \code{sm} is used to define the effect measure of interest for transforming the data into paired-preference format.
#' For binary outcomes the available effect measures are: (i) Odds ratio (sm="OR"), (ii) Risk ratio (sm="RR") and Risk difference (sm="RD").
#' For continuous outcomes the available effect measures are: (i) Mean Difference (sm="MD") and Standardized mean difference (sm="SMD").
#' 
#' 
#' @return
#' \itemize{
#' \item The initial data in a paired-preference format
#' \item The correspondence between the initial study names (passed in the argument studlab)
#' and the index name of the paired-preference format data
#' }
#' 
#' @references
#' \itemize{
#' \item Theodoros Evrenoglou, Adriani Nikolakopoulou, Guido Schwarzer, Gerta Rücker, Anna Chaimani (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic models and treatment-choice criteria.
#' \emph{https://arxiv.org/abs/2406.10612}
#' }



tcc <- function(treat,
                event,n,mean,sd,
                data=NULL,studlab,
                mcid=NULL,l.roe=NULL,u.roe=NULL,
                small.values="desirable",
                relax=F,sm,
                ...){
  
  require(meta)
  
  catch <- meta:::catch
  
  # Read the data from a data frame
  
  nulldata <- is.null(data)
  
  sfsp <- sys.frame(sys.parent())
  
  mc <- match.call()
  
  if (nulldata){
    
    data <- sfsp
    
  }
  
  studlab <- catch("studlab", mc, data, sfsp)
  
  treat <- catch("treat", mc, data, sfsp)
  
  event <- catch("event", mc, data, sfsp)
  
  mean <- catch("mean", mc, data, sfsp)
  
  sd <- catch("sd", mc, data, sfsp)
  
  n <- catch("n", mc, data, sfsp)
  
  args <- list(...) 
  
  nam.args <- names(args)
  
  if(missing(mcid)){
    
    if(missing(l.roe) & missing(u.roe)){
    
    stop("A range of equivalence (ROE) needs to be defined. For a symmetrical ROE you can specify it using the argument 'mcid' or you can explicitly define the ROE lower and upper limits using the arguments 'l.roe' and 'u.roe'.")
    
    }else if(missing(l.roe) & !missing(u.roe)){
        
      stop("Please, specify a lower limit for the range of equivalence.")
      
      }else if(!missing(l.roe) & missing(u.roe)){
        
        stop("Please, specify an upper limit for the range of equivalence.")
  
      }else{
        
        chknumeric(l.roe)
        
        chknumeric(u.roe)  
        
      }
  }else if(!missing(mcid)){
    
    chknumeric(mcid)  
    
  if(!missing(l.roe) & !missing(u.roe)){
    
  warning("The arguments 'l.roe' and 'u.roe' are deprecated since the argument 'mcid' was provided.")  
  
  }else if(!missing(l.roe) & missing(u.roe)){
    
    warning("The argument 'l.roe' is deprecated since the argument 'mcid' was provided.")  
    
  }else if(missing(l.roe) & !missing(u.roe)){
    
    warning("The argument 'u.roe' is deprecated since the argument 'mcid' was provided.")  
    
  }  
    
  }
  
  if(!missing(l.roe) & !missing(u.roe)){
    
  if((l.roe>u.roe) || (l.roe==u.roe)){
    stop("Argument 'l.roe' should always be smaller than 'u.roe.'")
  }
  
  }
  
  if(missing(small.values)){
    
    stop("Argument 'small.values' is missing. Please specify the type of outcome as either 'desirable' or 'undesirable'.")  
    
  }
  
  if(small.values %!in% c("desirable","undesirable")){
    
    stop("Unclear type of small values. Please specify as either 'desirable' or 'undesirable'.")
    
  }
  
  if(is.null(studlab)){
    
    stop("Argument 'studlab' mandatory.")
    
  }
  
  if (is.null(treat)){
    
    stop("Argument 'treat' mandatory.")
  }
  
  if (is.null(treat))
    stop("Argument 'treat' mandatory.")
  ##
  if (is.list(treat))
    chklist(treat)
  ##
  if (!is.null(event))
    if (is.list(event))
      chklist(event)
  else
    chknumeric(event)
  ##
  if (!is.null(n))
    if (is.list(n))
      chklist(n)
  else
    chknumeric(n)
  ##
  if (!is.null(mean))
    if (is.list(mean))
      chklist(mean)
  else
    chknumeric(mean)
  ##
  if (!is.null(sd))
    if (is.list(sd))
      chklist(sd)
  else
    chknumeric(sd)
  
  chklogical(relax)

  ### determine whether the outcome is binary or continuous
  
  if(!is.null(event) & !is.null(n) &
     is.null(mean) & is.null(sd)){
    
    type <- "binary"
    
  } else if (!is.null(n) & !is.null(mean) & !is.null(sd)){
    
    type <- "continuous"
    
  }else{
    
    stop("Type of outcome unclear. Please provide the necessary ",
         "information:\n  - event, n (binary outcome)\n  - n, ",
         "mean, sd (continuous outcome).")
    
    
  }
  
  
  ##### Determine the format of the data
  
  data.format <- NA
  
  if(type=="binary"){
    
    if(is.list(event) & is.list(n) & is.list(treat)){
      
      if((length(event)!=length(n)) || (length(event)!=length(treat)) || (length(treat)!=length(n))){
        
        stop("List arguments 'treat', 'event' and 'n' must have the same length.") 
      } 
      
      if (length(event) == 1 & length(n) == 1 & length(treat) == 1) {
        
        data.format <- "long"
        
        event <- unlist(event)
        
        n <- unlist(n)
        
        treat <- unlist(treat)
        
      }else if(length(event) > 1 & length(n) > 1 & length(treat) > 1){
        
        data.format <- "wide" ## don't unlist here 
        
      } 
      
    }else if (is.numeric(event) & is.numeric(n)){
      
      data.format <- "long"
      
    }
    
  } else if (type=="continuous"){
    
    if(is.list(mean) & is.list(sd) &  is.list(n) & is.list(treat)){
      
      if((length(mean)!=length(n)) || (length(mean)!=length(treat)) || (length(mean)!=length(sd)) || (length(n)!=length(sd)) || (length(n)!=length(treat)) || (length(sd)!=length(treat))  ){
        
        stop("List arguments 'treat', 'mean', 'sd' and 'n' must have the same length.") 
      } 
      
      if (length(mean) == 1 & length(sd) == 1 & length(n) == 1 & length(treat) == 1) {
        
        data.format <- "long"
        
        mean <- unlist(mean)
        
        n <- unlist(n)
        
        sd <- unlist(sd)
        
        treat <- unlist(treat)
        
      }else if(length(mean) > 1 & length(sd) > 1 & length(n) > 1 & length(treat) > 1){
        
        data.format <- "wide" ## don't unlist here 
        
      } 
      
    }else if (is.numeric(mean) & is.numeric(sd) &  is.numeric(n)){
      
      data.format <- "long"
      
    }  
    
  }
  
  
  if(data.format=="long"){
    
    if(sm %in% c("RR","OR","RD")){
      
      data_new <- cbind.data.frame("studlab" = studlab,"treat" = treat,"event" = event,"n" = n)
      
      data_new <- clean(data = data_new,outcome = type) ### clean for studies with zero events in all arms or missing number of events and sample sizes
      
      data_new$studlab_orig <- data_new$studlab
      
      data_new <- transform(data_new,studlab = as.numeric(factor(studlab)))
      
    }else{
      
      data_new <- cbind.data.frame("studlab" = studlab,"treat" = treat,"mean" = mean,"sd"=sd,"n" = n)
      
      data_new <- clean(data = data_new,outcome = type) ### clean studies with missing number of means and sample sizes
      
      data_new$studlab_orig <- data_new$studlab
      
      data_new <- transform(data_new,studlab = as.numeric(factor(studlab)))
      
    }
    
  }else if (data.format=="wide"){
    
    if(sm %in% c("RR","OR","RD")){
      
      data_new <- go_long(studlab = studlab,treat = treat,event = event,n=n,type=type)
      
      data_new <- clean(data = data_new,outcome = type) ### clean for studies with zero events in all arms or missing number of events and sample sizes
      
      data_new$studlab_orig <- data_new$studlab
      
      data_new <- transform(data_new,studlab = as.numeric(factor(studlab)))
      
      
    } else{
      
      data_new <- go_long(studlab = studlab,treat = treat,mean = mean,n=n,sd=sd,type=type)
      
      data_new <- clean(data = data_new,outcome = type) ### clean studies with missing number of means and sample sizes
      
      data_new$studlab_orig <- data_new$studlab
      
      data_new <- transform(data_new,studlab = as.numeric(factor(studlab)))
      
    }
    
  }
  
  ## use pairwise to transform the long format data to contrasts using the pairwise() function from netmeta.
  ## this also ensures that a network of multi-arm studies gets an equivalent representation of a network of two-arm studies.
  
  if(sm %in% c("RR","OR","RD")){
    
    dat_new <- pairwise(data = data_new,treat = treat,event = event,n = n,sm = sm,studlab = studlab, ...)
    
  }else{
    
    dat_new <- pairwise(data = data_new,treat = treat,mean = mean,sd = sd,n = n,sm = sm,studlab = studlab, ...)
    
  }
  
  dat_new$id <- 1:nrow(dat_new)
  
  u <- dat_new$id
  
  r <- list()
  
  r1 <- list()
  
  x <- list()
  
  rank_final <- list()
  
  ind <- list()
  
  index <- list()
  
  #### Define the Range of Equivalence based on (i) the 'civ' argument which automatically produces
  #### a symmetrical ROE or (ii) by specifying the bounds of the ROE based on the arguments 'l.roe'
  #### and 'u.roe'.
  
  if(is.null(mcid)){
  
  roe <- c(l.roe, u.roe)
  
  }else{
    if(sm %in% c("RR","OR")){

  if(mcid==1){
    
  l.roe <- u.roe <- 1
  
  warning("A minimal clinically important difference equal to 1 results in a range of equivalence (ROE) 
          with both bounds equal to 1.") 
     
  }else if(mcid==0){
    
    l.roe <- u.roe <- 0
    
    warning("A minimal clinically important difference equal to 0 results in a range of equivalence (ROE) 
          with both bounds equal to 0.") 
    
  }else{
                
  l.roe <- min(mcid,1/mcid)
  
  u.roe <- max(mcid,1/mcid)
  
  }
      
    }else{
  
      l.roe <- min(mcid,-mcid)
      
      u.roe <- max(mcid,-mcid)
      
    }
    
    roe <- c(l.roe,u.roe)
    
    roe <- round(roe,digits = 2)
            
  }
  
  
  
  ## define the treatment choice criterion
  ## for that we need: (i) what is the null effect, (ii) how the ROE of equivalence will be treated
  
  ## define the null effect, for ratio measures (OR and RR) that is 1 and for difference measures (i.e MD,SMD and RD) this should be 0.
  
  no_effect_sign <- ifelse(sm %in% c("OR","RR"),1,0) 
  
  ## the argument relax defines how the ROE should be treated. The default is FALSE. If TRUE, it relaxes the TTC shown in the main manuscript.
  
  ## The modified tcc does not account for the statistical significance of the effect. Now for a relative effect to indicate a "win" 
  ## it requires that the relative effect (point estimate) and one bound of the confidence interval will be clinically significant (i.e. outside the ROE) 
  ## and the other bound simply does not indicate a clinically significant effect on the opposite direction.
  
  if(relax==T){
    
    no_effect1 <- l.roe
    
    no_effect2 <- u.roe
    
    ## usual tcc as described in the manuscript
  }else{
    
    no_effect1 <- no_effect2 <- no_effect_sign
    
  }
  
  ### Define wins and ties for each study specific pairwise comparison
  
  for(i in 1:length(u)){
    
    r[[i]] <- dat_new %>%    ### each "i" in the list r[[]] is a row of the dataset obtained from the pairwise function. 
      filter(id==u[[i]])     ### therefore the list r[[]] contains all the study-specific pairwise comparisons
    
    r1[[i]] <- r[[i]] %>%   ### keep only the elements required for the tcc. These are: (i) the study id, (ii) the treatment labels, (iii) the treatments effects, (iv) the treatment effect uncertainty 
      dplyr::select(studlab, treat1, treat2, TE, seTE,studlab_orig)
    
    ## calculate 95% Wald type confidence intervals for each study specific pairwise comparison
    r1[[i]]$L <- r1[[i]]$TE - qnorm(0.975)*r1[[i]]$seTE
    
    r1[[i]]$U <- r1[[i]]$TE + qnorm(0.975)*r1[[i]]$seTE
    
    ## for the case of a binary outcome and when the user indicates "RR" or "OR" as effect measure the pairwise() function returns them in a log-scale
    ## here we transform them into the natural scale for both the point estimates and the 95% confidence interval bounds. 
    ## In any other case of effect measures no other transformation takes place.
    
    if(sm %in% c("RR","OR")){
      
      r1[[i]]$es <- exp(r1[[i]]$TE)
      
      r1[[i]]$l.es <- exp(r1[[i]]$L)
      
      r1[[i]]$u.es <- exp(r1[[i]]$U)
      
    }else{
      
      r1[[i]]$es <- r1[[i]]$TE
      
      r1[[i]]$l.es <- r1[[i]]$L
      
      r1[[i]]$u.es <- r1[[i]]$U
    }
    
    r1[[i]]$rank_text <- NA
    
    r1[[i]]$r_t1 <- NA
    
    r1[[i]]$r_t2 <- NA
    
    #### Define tcc for the case where small.values are 'undesirable' and for the case that they are 'desirable'
    
    if(small.values=="undesirable"){ ## since 'small.values' are undesirable the larger the treatment effect the more likely that for the comparison t1 vs t2, t1 is preferred.
      
      for (j in 1:nrow(r1[[i]])){
        
        if(r1[[i]]$u.es[j] < min(roe)){
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat2[j]," > ",r1[[i]]$treat1[j],sep = "")
          
          ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the second position and the treatment 2 in the first position (i.e. t2>t1)
          r1[[i]]$r_t1[j] <- 2 
          
          r1[[i]]$r_t2[j] <- 1
        }
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the first position and the treatment 2 in the second position (i.e. t1>t2)
        else if(r1[[i]]$l.es[j] > max(roe)){
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat1[j]," > ",r1[[i]]$treat2[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 1 
          
          r1[[i]]$r_t2[j] <- 2
          
        }
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the second position and the treatment 2 in the first position (i.e. t2>t1)
        ## here 'no_effect2' can be either the null value (if relax=FALSE (default)) or it can be the upper bound of ROE if relax=T.
        ## the relaxed criterion allows for more "wins" to be identified from the TTC as in this case the statistical significance of the effect does not alter the preference
        
        else if(r1[[i]]$l.es[j] < min(roe) & (r1[[i]]$es[j] < min(roe)) & (r1[[i]]$u.es[j] < no_effect2)){ ## can be combined with first case
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat2[j]," > ",r1[[i]]$treat1[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 2 
          
          r1[[i]]$r_t2[j] <- 1
          
        }
        
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the first position and the treatment 2 in the second position (i.e. t1>t2)
        ## here 'no_effect1' can be either the null value (if relax=FALSE (default)) or it can be the lower bound of ROE if relax=T.
        ## the relaxed criterion allows for more "wins" to be identified from the TTC as in this case the statistical significance of the effect does not alter the preference
        
        else if(r1[[i]]$u.es[j] > max(roe) & (r1[[i]]$es[j] > max(roe)) & (r1[[i]]$l.es[j] > no_effect1)){ ## can be combined with second case
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat1[j]," > ",r1[[i]]$treat2[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 1 
          
          r1[[i]]$r_t2[j] <- 2
          
          #### in any other case the tcc indicates a tie  
        }else{
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat1[j]," = ",r1[[i]]$treat2[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 1
          
          r1[[i]]$r_t2[j] <- 1
        }
        
      }
      
      ### repeat the process for the case where 'small.values=desirable'
      
    }else if (small.values=="desirable") { ## since 'small.values' are desirable the smaller the treatment effect the more likely that for the comparison t1 vs t2, t1 is preferred.
      
      for (j in 1:nrow(r1[[i]])){
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the first position and the treatment 2 in the second position (i.e. t1>t2)
        if(r1[[i]]$u.es[j] < min(roe)){
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat1[j]," > ",r1[[i]]$treat2[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 1 
          
          r1[[i]]$r_t2[j] <- 2
        }
        else if(r1[[i]]$l.es[j] > max(roe)){
          
          ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the second position and the treatment 2 in the first position (i.e. t2>t1)
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat2[j]," > ",r1[[i]]$treat1[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 2 
          
          r1[[i]]$r_t2[j] <- 1
          
        }
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the first position and the treatment 2 in the second position (i.e. t1>t2)
        ## here 'no_effect2' can be either the null value (if relax=FALSE (default)) or it can be the upper bound of ROE if relax=T.
        ## the relaxed criterion allows for more "wins" to be identified from the TTC as in this case the statistical significance of the effect does not alter the preference
        
        else if(r1[[i]]$l.es[j] < min(roe) & (r1[[i]]$es[j] < min(roe)) & (r1[[i]]$u.es[j] < no_effect2)){ ## can be combined with first case
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat1[j]," > ",r1[[i]]$treat2[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 1 
          
          r1[[i]]$r_t2[j] <- 2
          
        }
        
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the second position and the treatment 2 in the first position (i.e. t2>t1)
        ## here 'no_effect1' can be either the null value (if relax=FALSE (default)) or it can be the upper bound of ROE if relax=T.
        ## the relaxed criterion allows for more "wins" to be identified from the TTC as in this case the statistical significance of the effect does not alter the preference
        
        else if((r1[[i]]$u.es[j] > max(roe)) & (r1[[i]]$es[j] > max(roe)) & (r1[[i]]$l.es[j] > no_effect1)){ ## can be combined with second case
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat2[j]," > ",r1[[i]]$treat1[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 2 
          
          r1[[i]]$r_t2[j] <- 1
          
          #### in any other case the tcc indicates a tie 
        }else{
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat1[j]," = ",r1[[i]]$treat2[j],sep = "")
          
          r1[[i]]$r_t1[j] <- 1 
          
          r1[[i]]$r_t2[j] <- 1
        }
        
      }
    }
    
    ####################################################
    
    ## END OF tcc ## 
    
    ## Gather the results across all pairwise comparisons
    #################################################
    
    x[[i]] <- cbind.data.frame(
      rep(r1[[i]]$studlab,2),
      rep(r1[[i]]$studlab_orig,2),
      rbind.data.frame(r1[[i]]$treat1,r1[[i]]$treat2),
      rbind.data.frame(r1[[i]]$r_t1,r1[[i]]$r_t2)
    )
    
    names(x[[i]]) <- c("studlab","studlab_orig","treat","rank")
    
    x[[i]] <- x[[i]] %>% 
      arrange(x[[i]]$rank)
    
    x[[i]]$outcome <- ifelse(sum(x[[i]]$rank)==2,"tie","winner")
    
    ind[[i]] <- paste(unique(x[[i]]$outcome),".",i,sep="")
    
  }
  
  #### create a data frame in a wide format with the information for all comparisons   
  data_pairwise <- bind_rows(r1, .id = "column_label")
  
  data_pairwise$column_label <- NULL
  
  ### check if only ties were identified from tcc and return a warning if so. In this case the model fit from mtrank() will fail to converge.
  all.ties <- ifelse(unique(data_pairwise$r_t1+data_pairwise$r_t2)==2,TRUE,FALSE)
  
  if(isTRUE(all.ties)){
    
    warning("Only ties were identified through the treatment choice criterion. This can yield into convergence problems when using the mtrank() function.")
    
  }  
  
  ### organize the list x so that all studies with "ties" come first and studies with "wins" second
  ### this is because the rankings() function that is used to perform the final transformation requires (or its a bug?) this
  
  ind_f <- unlist(ind)
  
  names(x) <- as.character(ind_f)
  
  x <- x[order(names(x))]
  
  names(x) <- NULL
  
  ## save the order of the studies and use it for the "as.rankings" function to group the preferences
  
  for(i in 1:length(x)){
    
    index[[i]] <- unique(x[[i]]$studlab)
    
  }
  
  index <- unlist(index)
  
  ### create a dataframe with all the elements of list x
  data_final <- bind_rows(x, .id = "column_label")
  
  data_final$column_label <- as.numeric(data_final$column_label)
  
  ### ungrouped treatment preferences
  ## the number of rows is equal to the number of study-specific pairwise comparisons in the network
  
  rankings <- PlackettLuce::rankings(data_final,
                                     id = "column_label",
                                     item = "treat",
                                     rank = "rank"
  )
  
  ### ensure that preferences from multi-arm studies are grouped together using the grouping criterion based on "index"
  ### the number of rows is equal to the number of studies in the network
  
  ### the output of this function gives the grouped study preferences. The order that the comparisons appear correspond to the
  ### order of rows in the 'index_match' which is created below.
  
  res <- PlackettLuce::as.rankings(rankings,index = index)
  
  ## save the results (i) preference.data: the data transformed in a preference forma
  ##                  (ii) index.match: a dataframe which contains the matching in the names of "preference.data" and the actual study names,
  ##                                    this is helpful to keep track on which study of the original data generates the ith preference of the "preference.data" output.              
  
  
  
  ## match the study index with the original study name 
  
  index_match <- cbind.data.frame("index"=data_final$studlab,"study_name"=data_final$studlab_orig)
  
  index_match <- index_match[!duplicated(index_match$index,index_match$study_name), ]
  
  index_match  <- index_match[order(as.numeric(index_match$index)),]
  
  row.names(index_match) <- NULL
  
  ######## Createthe final output and the attributes ####
  
  res1 <- list("preference.data"=res,"index.match"=index_match)
  
  attr(res1,"data_pairwise") <- data_pairwise
  
  attr(res1,"ROE") <- roe
  
  attr(res1,"sm") <- sm
  
  attr(res1,"ungrouped.preferences") <- rankings
  
  attr(res1,"all.ties") <- all.ties
  
  attributes(res1) <- c(attributes(res1),attributes(res))
  
  class(res1) <- c("mtrank",class(res))
  
  return(res1)
  
}