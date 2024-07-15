#' Vizualize the study-specific preferences or ties generated from the treatment choice criterion defined by the function \code{\link{TCC}}.
#' 
#' @description
#' This function produces a forest plot for all (or some specific) study specific comparisons and vizualizes the treatment preference or ties
#' which are defined from the treatment choice criterion which is constructed from the function \code{\link{TCC}}.
#' 
#' @param x An object of class "mtrank" generated as the output of the \code{\link{TCC}} function. 
#' @param treat A treatment of interest. If specified it returns a forest plot for all study specific effects
#' related to \code{treat}. If NULL (default) it generated forest plots for all study-specific effects in the network. 

#'   
#' @details  
#' This function produces forest plots for the study specific treatment effects in the network. The legend of these graphs specifies whic
#' treatment effects were identified as treatment preferences and which as treatment ties. Additionally, the respective range of equivalence
#' defined at the function \code{\link{TCC}} is also vizualized for each forest plot. The argument \code{treat} is optional. If specified 
#' it returns only the forest plots in terms of the study-specific treatment effects related to the specified \code{treat}. If NULL it will 
#' return the forest plots in terms of all study-specific treatment effects in the network. We recommend that the specification of the argument 
#' \code{treat} especially for busy networks which contain many direct comparisons. 
#' 
#' @return
#' A forest plot that shows the study-specific treatment effects and the range of equivalence defined from the function \code{\link{TCC}}. Coloured
#' with red are the treatment effects which according to the \code{\link{TCC}} function were producing a treatment preference and coloured with
#' black those treatment effects producing a treatment tie.
#' 
#' @references
#' \itemize{
#' \item Theodoros Evrenoglou, Adriani Nikolakopoulou, Guido Schwarzer, Gerta Rücker, Anna Chaimani (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic models and treatment-choice criteria.
#' \emph{https://arxiv.org/abs/2406.10612}
#' }




viz.mtrank <- function(x,treat=NULL){

  if(inherits(x, "mtrank")){
    
  data_wide <- attributes(x)$data_pairwise   
  
  ROE <- attributes(x)$ROE
  
  sm <- attributes(x)$sm
  
  l.roe <- min(ROE)
  
  u.roe <- max(ROE)
  
  data_wide$comp <- paste(data_wide$treat1,data_wide$treat2,sep = " vs ")
  
  all_treats <- c(unique(data_wide$treat1),unique(data_wide$treat2))
  
  all_treats <- unique(all_treats)
  
  }else{
    
  stop("Please, check your 'data' argument as currently it is not of 'mtrank' class.")  
  }
  
  
 if(!is.null(treat) && (treat %!in% all_treats)){
   
  stop("Unknown treatment name.") 
 }

if(is.null(treat)){
  
  data_wide <- data_wide
  
}else{
  
  data_wide <- data_wide %>% 
  filter(grepl(paste(treat), comp))

  }
  

data_wide$outcome <- ifelse(data_wide$r_t1+data_wide$r_t2==2,"tie","win") 

data_wide$color <- ifelse(data_wide$outcome=="win","red","black")
  
r <- list()

mod <- list()

u <- unique(data_wide$comp)

for(i in 1:length(u)){
  
r[[i]] <- data_wide %>% 
filter(comp==u[i])


mod[[i]] <- rma(data = r[[i]],
                yi = TE,
                sei=seTE,
                method = "FE",
                slab = studlab_orig
                )    

par(ask=T)

if(sm %in% c("RR","OR")){

  metafor::forest(mod[[i]],
                atransf=exp,
                addfit=F,
                steps=4,
                xlab="",
                psize=1,
                colout=r[[i]]$color
                )

}else{
  
  metafor::forest(mod[[i]],
                  addfit=F,
                  steps=4,
                  xlab="",
                  psize=1,
                  colout=r[[i]]$color
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


par(font=2)

text(x=mean(par.usr[1:2]),
    y=mod[[i]]$k+2,
    paste("Comparison: ",unique(r[[i]]$comp),sep = ""),
    col="black"
    )

if(length(unique(r[[i]]$color))==2){
  
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
} else if((length(unique(r[[i]]$color))==1) & (unique(r[[i]]$color)=="black")){
  
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
  
  
  
} else if((length(unique(r[[i]]$color))==1) & (unique(r[[i]]$color)=="red")){
  
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

par(font=1)

par(ask=FALSE)

}

  
}




