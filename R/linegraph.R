#' Line graph plotting the results of \code{\link{mtrank}} across different MCID values. 
#' 
#' @description
#' This function produces a line graph that visualizes the results of \code{\link{mtrank}} 
#' in terms of either abilities or probabilities across different MCID values.
#' 
#' @param x An object of class \code{\link{netmeta}}.
#' @param mcid.vec A numeric vector of MCID values to be used for the sensitivity analysis.
#' @param mcid.ref A numeric MCID value to be used as the reference for sorting treatments in the final graph. 
#'   This value should be included in \code{mcid.vec}.
#' @param small.values A character string specifying whether small treatment effects indicate a 
#'   beneficial (\code{"desirable"}) or harmful (\code{"undesirable"}) effect.
#' @param type The metric to be used for plotting the results of the sensitivity analysis. 
#'   Two options are available: the default is \code{"probability"}, which plots results in terms of normalized abilities; 
#'   the alternative is \code{"ability"}, which plots results in terms of ability estimates. Both options can be abbreviated. 
#' @param k A numeric value indicating the number of treatments to be plotted. By default, all available treatments are shown. 
#'   For large networks, it is advisable to limit the number of treatments to improve readability. If specified, the first \code{k} 
#'   treatments based on the hierarchy at \code{mcid.ref} will be plotted. 
#' @param backtransf A logical value indicating whether to display log-ability estimates (\code{FALSE}, default) 
#'   or back-transformed ability estimates on the natural scale (\code{TRUE}). This argument is ignored if \code{type = "probability"}.
#' @param linewidth A numeric value specifying the width of the lines (default: 1.1).
#' @param point.size A numeric value specifying the size of the points (default: 2).
#' @param \dots Additional arguments passed to \code{\link[mtrank]{mtrank}} or \code{\link[ggplot2]{ggplot}}.
#' 
#' @details
#' This function creates a line graph to visualize probability or ability estimates
#' obtained from \code{\link{mtrank}} across different MCID values. The order of treatments in the
#' graph is based on their hierarchy at the reference MCID value (\code{mcid.ref}).
#' 
#' @return
#' A \code{ggplot} object.
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Ruecker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#'
##'
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
#' # Perform a sensitivity analysis across different MCID values assuming that 1.20 is the reference value
#' mcid.vec <- seq(1.10,1.50,by=0.10)
#' mcid.ref <- 1.20
#' # plot all the treatments in the network
#' linegraph(net,
#'          mcid.vec = mcid.vec,
#'          mcid.ref = mcid.ref,
#'          small.values = "undesirable"   
#' )
#' # plot in terms of ability estimates 
#' linegraph(net,
#'         mcid.vec = mcid.vec,
#'         mcid.ref = mcid.ref,
#'         small.values = "undesirable",
#'         type = "ability"
#' )
#' # plot only the first three treatments in the order appearing at the 'mcid.ref' value
#' linegraph(net,
#'          mcid.vec = mcid.vec,
#'          mcid.ref = mcid.ref,
#'          k = 3,
#'          small.values = "undesirable"   
#' )
#'
#' @export


linegraph <- function(x, 
                       mcid.vec,
                       mcid.ref,
                       small.values = x$small.values,
                       type = "probability",
                       k = length(x$trts),
                       backtransf = FALSE,
                       linewidth=1.1,
                       point.size=2,
                       ...){
  #
  chkclass(x, "netmeta")
  chknumeric(mcid.vec)
  chknumeric(mcid.ref)
  chknumeric(k,min=0)
  #
  type <- setchar(type,val=c("ability","probability"))

  #
  E <- which(abs(mcid.vec - mcid.ref) < 1e-8)
  
  if(length(E)==0){
    
    stop("The reference MCID value should be part of the MCID values in 'mcid.vec'.")
  }
  
  mod <- r <- ests <- prob <- vector("list")
  
  #
  for(i in 1:length(mcid.vec)){
    
    r[[i]] <- tcc(x,
                  small.values = small.values,mcid = mcid.vec[i])
    
    
    if(isFALSE(r[[i]]$all.ties)){
      
      mod[[i]] <-   mtrank(r[[i]],...)
      
      
      prob[[i]] <- mod[[i]]$probabilities
      
      
      prob[[i]]$mcid <- mcid.vec[i]
      
      ests[[i]] <- mod[[i]]$estimates %>% 
        arrange(desc(log_ability)) %>% 
        mutate(mcid=mcid.vec[i])
    }
    
    else{
      
      prob[[i]] <- NA
      
      ests[[i]] <- NA
      
    }
    
  }
  
  prob <- Filter(function(x) !is.null(x) && !(is.atomic(x) && all(is.na(x))), prob)
  
  ests <- Filter(function(x) !is.null(x) && !(is.atomic(x) && all(is.na(x))), ests)
  
  #
  prob_new <- bind_rows(prob)
  
  ests_new <- bind_rows(ests)
  
  treats <- prob[[E]]$treatment[1:k]
  
  if(type=="probability"){
    
    data <- prob_new %>% 
      filter(treatment %in% treats) %>% 
      mutate(treatment = factor(treatment,levels = treats))
    
    graph <- ggplot(data, aes(x = mcid, y = probability, color = treatment)) +
      geom_line(linewidth = linewidth) +
      geom_point(size = point.size)+
      theme_minimal()+
      xlab("MCID")+
      ylab("Probability")+
      ylim(c(0,1))+
      scale_x_continuous(breaks = mcid.vec) +
      guides(color=guide_legend(title="Treatment"))
  }
  else if(type=="ability"){
    
    data <- ests_new %>% 
      filter(treatment %in% treats) %>% 
      mutate(treatment = factor(treatment,levels = treats))
    
    if(isFALSE(backtransf)){
      graph <- ggplot(data, aes(x = mcid, y = log_ability, color = treatment)) +
        geom_line(linewidth = linewidth) +
        geom_point(size = point.size)+
        theme_minimal()+
        xlab("MCID")+
        ylab("log-abilities")+
        scale_x_continuous(breaks = mcid.vec) +
        guides(color=guide_legend(title="Treatment"))
    }
    else{
      graph <- ggplot(data, aes(x = mcid, y = exp(log_ability), color = treatment)) +
        geom_line(linewidth = linewidth) +
        geom_point(size = point.size)+
        theme_minimal()+
        xlab("MCID")+
        ylab("Abilities")+
        scale_x_continuous(breaks = mcid.vec) +
        guides(color=guide_legend(title="Treatment"))
      
    }
    
  }
  
  E_mcid <- which(!mcid.vec %in% unique(data$mcid))
  
  mcid_all_ties <- unique(mcid.vec)[E_mcid] 
  
  if(length(E_mcid)!=0){
    
    txt_mcid <- paste(mcid_all_ties,collapse = ", ")  
    
    warning(paste("Values ",txt_mcid," are not shown as for these the treatment choice criterion identified only ties."))  
    
  }
  
  attr(graph,"data") <- data
  
  return(graph)
  
  
  
}
