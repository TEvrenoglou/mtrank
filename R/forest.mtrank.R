#' Vizualize ability estimates produced from the function \code{\link{mtrank}}.
#' 
#' @description
#' This function produces a forest plot that vizualizes the ability estimates produced from the output of the R function \code{\link{mtrank}}.
#' 
#' @param x An object of class "mtrank.fit" generated as the output of the \code{\link{mtrank}} function. 
#' @param sorting An argument specifying the criterion for sorting the ability estimates in the forest plot (see Details).
#' @param backtransf An argument specifying the scale of the ability estimates. If FALSE (default) the forest plot vizualizes 
#' the log-ability estimates. If TRUE the scale is transformed to the natural scale of ability estimates. 
#' @param \dots Additional arguments.
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
#' @method forest mtrank
#' @export


forest.mtrank <- function(x, sorting = "ability", backtransf = FALSE, ...) {
  
  chkclass(x, "mtrank")
  
  dat <- x$estimates
  reference.group <- x$reference.group
  #
  sorting <- setchar(sorting, c("ability", "se", "none"))
  chklogical(backtransf)
  
  #
  # order data   
  #
  if (sorting == "ability")
    o <- order(-dat$log_ability)
  else if (sorting == "se")
    o <- order(dat$se)
  else if (sorting == "none")
    o <- seq_len(nrow(dat))
  #
  dat <- dat[o, , drop = FALSE]
    
  if (!is.null(reference.group))
    dat <- dat[complete.cases(dat$se), , drop = FALSE]

  #
  # Fit a pairwise meta-analysis model
  #
  model.rma <- rma(yi = dat$log_ability, sei = dat$se,
                   slab = dat$treatment)
  
  #### 
  
  if (backtransf)
    xlab <- "Ability [95% CI]"
  else
    xlab <- "Log-Ability [95% CI]"
  
  ### create the forest plot   
  
  if (backtransf)
    forest(model.rma,
           atransf=exp,
           psize=1,
           addfit=F,
           mlab="",
           xlab=xlab)
  else
    forest(model.rma,
           psize=1,
           addfit=F,
           mlab="",
           xlab=xlab)
  #
  invisible(NULL)
}
