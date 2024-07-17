#' Vizualize the study-specific preferences or ties generated from the treatment choice criterion defined by the function \code{\link{tcc}}.
#' 
#' @description
#' This function produces a forest plot for all (or some specific) study specific comparisons and vizualizes the treatment preference or ties
#' which are defined from the treatment choice criterion which is constructed from the function \code{\link{tcc}}.
#' 
#' @param x An object of class "mtrank" generated as the output of the \code{\link{tcc}} function. 
#' @param treat A treatment of interest. If specified it returns a forest plot for all study specific effects
#' related to \code{treat}. If NULL (default) it generated forest plots for all study-specific effects in the network. 
#' @param \dots Additional arguments.
#'   
#' @details  
#' This function produces forest plots for the study specific treatment effects in the network. The legend of these graphs specifies whic
#' treatment effects were identified as treatment preferences and which as treatment ties. Additionally, the respective range of equivalence
#' defined at the function \code{\link{tcc}} is also vizualized for each forest plot. The argument \code{treat} is optional. If specified 
#' it returns only the forest plots in terms of the study-specific treatment effects related to the specified \code{treat}. If NULL it will 
#' return the forest plots in terms of all study-specific treatment effects in the network. We recommend that the specification of the argument 
#' \code{treat} especially for busy networks which contain many direct comparisons. 
#' 
#' @return
#' A forest plot that shows the study-specific treatment effects and the range of equivalence defined from the function \code{\link{tcc}}. Coloured
#' with red are the treatment effects which according to the \code{\link{tcc}} function were producing a treatment preference and coloured with
#' black those treatment effects producing a treatment tie.
#' 
#' @references
#' Evrenoglou E, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#'
#' @examples
#'  \dontrun{
#'  # Add examples
#' }
#' 
#' @method forest tcc
#' @export


forest.tcc <- function(x, treat = NULL, ...) {

  chkclass(x, "tcc")
  
  data_wide <- x$data_wide   
  roe <- x$roe
  sm <- x$sm
  #
  l.roe <- min(roe)
  u.roe <- max(roe)
  #
  data_wide$comparison <-
    paste(data_wide$treat1, data_wide$treat2, sep = " vs ")
  #
  trts <- x$trts
  
  treat <- setchar(treat, trts)
  #
  if (is.null(treat))
    data_wide <- data_wide
  else
    data_wide <- data_wide %>% filter(grepl(treat, comparison))
  
  data_wide$outcome <-
    ifelse(data_wide$r_t1 + data_wide$r_t2 == 2, "tie","win") 
  
  data_wide$color <- ifelse(data_wide$outcome == "win", "red", "black")
  
  # Get rid of warning 'no visible binding for global variable'
  comparison <- NULL
  
  dat <- list()
  
  mod <- list()
  
  u <- unique(data_wide$comparison)
  
  for (i in 1:length(u)) {
    
    dat[[i]] <- data_wide %>% filter(comparison == u[i])
    
    
    mod[[i]] <- rma(yi = dat[[i]]$TE,
                    sei = dat[[i]]$seTE,
                    method = "FE",
                    slab = dat[[i]]$studlab_orig)
    
    if (sm %in% c("RR","OR")) {
      
      metafor::forest(mod[[i]],
                      atransf=exp,
                      addfit=F,
                      steps=4,
                      xlab="",
                      psize=1,
                      colout = dat[[i]]$color
      )
      
    }
    else {
      metafor::forest(mod[[i]],
                      addfit=F,
                      steps=4,
                      xlab="",
                      psize=1,
                      colout = dat[[i]]$color
      )
      
      
    }  

    par.usr <- par("usr")
    
    l.x0 <- ifelse(sm %in% c("RR","OR"),log(l.roe),l.roe) 
    
    u.x0 <- ifelse(sm %in% c("RR","OR"),log(u.roe),u.roe) 
    
    segments(x0=l.x0, 
             y0=par.usr[3],
             x1 = l.x0, 
             y1 = mod[[i]]$k+1,
             lty="dotted",
             lwd=2,
             col = "darkblue")
    
    
    segments(x0=u.x0, 
             y0=par.usr[3],
             x1 = u.x0, 
             y1 = mod[[i]]$k+1,
             lty="dotted",
             lwd=2,
             col = "darkblue")

    text(x=mean(par.usr[1:2]),
         y=mod[[i]]$k+2,
         paste("Comparison: ",unique(dat[[i]]$comparison),sep = ""),
         col="black"
    )
    
    if (length(unique(dat[[i]]$color)) == 2) {
      
      legend("bottomleft", 
             legend=c("Treatment preference","Treatment tie"),
             col = c("red","black"),
             lty=1,
             lwd=2,
             box.lty = 0,
             box.lwd = 1,
             box.col = "black",
             cex = 0.7
      )
    }
    else if ((length(unique(dat[[i]]$color)) == 1) & (unique(dat[[i]]$color) == "black")) {
      
      legend("bottomleft", 
             legend=c("Treatment tie"),
             col = c("black"),
             lty=1,
             lwd=2,
             box.lty = 0,
             box.lwd = 1,
             box.col = "black",
             cex = 0.7
      )
      
      
      
    }
    else if ((length(unique(dat[[i]]$color)) == 1) & (unique(dat[[i]]$color) == "red")) {
      
      legend("bottomleft", 
             legend=c("Treatment preference"),
             col = c("red"),
             lty=1,
             lwd=2,
             box.lty = 0,
             box.lwd = 1,
             box.col = "black",
             cex = 0.7
      )
    }
  }
  
 invisible(NULL)
}
