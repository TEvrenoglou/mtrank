#' Forest plot of ability estimates produced with \code{\link{mtrank}}
#' 
#' @description
#' This function produces a forest plot that visualizes the ability estimates
#' calculated with \code{\link{mtrank}}.
#' 
#' @param x An object of class \code{\link{mtrank}}.
#' @param sorting An argument specifying the criterion to sort the ability
#' estimates in the forest plot (see Details).
#' @param backtransf A logical argument specifying whether to show log-ability
#'   estimates (\code{FALSE}, default) or ability estimates on the natural
#'   scale (\code{TRUE}).
#' @param xlab A label for the x-axis.
#' @param leftcols A character vector specifying columns
#'   to be printed on the left side of the forest plot
#'   (see \code{\link[meta]{forest.meta}}).
#' @param rightcols A character vector specifying columns
#'   to be printed on the right side of the forest plot
#'   (see \code{\link[meta]{forest.meta}}).
#' @param leftlabs A character vector specifying labels for
#'   columns on left side of the forest plot.
#' @param rightlabs A character vector specifying labels for
#'   columns on right side of the forest plot.
#' @param label.left Graph label on left side of null effect.
#' @param label.right Graph label on right side of null effect.
#' @param header.line A logical value indicating whether to print a
#'   header line or a character string ("both", "below", "").
#' @param \dots Additional arguments (passed on to
#'   \code{\link[meta]{forest.meta}}).
#' 
#' @details
#' The function produces a forest plot and visualizes the ability estimates
#' obtained from \code{\link{mtrank}}. The order of the estimates in the
#' forest plot (argument \code{sorting}) can be one of the following:
#' \itemize{
#' \item "ability": sort by descending ability estimates (default),
#' \item "se": sort by descending precision, i.e., increasing standard errors,
#' \item "none": use order from data set.
#' }
#' 
#' @return
#' A forest plot is plotted in the active graphics device.
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#'
#' @keywords hplot
##'
#' @examples
#' data(antidepressants)
#' #
#' ranks <- tcc(treat = drug_name, studlab = studyid,
#'   event = responders, n = ntotal, data = antidepressants,
#'   mcid = 1.25, sm = "OR", small.values = "undesirable")
#' #
#' fit <- mtrank(ranks)
#' 
#' forest(fit, treat = "escitalopram")
#' 
#' @method forest mtrank
#' @export

forest.mtrank <- function(x, sorting = "ability", backtransf = FALSE,
                          xlab = "",
                          leftcols = "studlab", leftlabs = "Treatment",
                          rightcols = c("effect", "ci"),
                          rightlabs =
                            c(paste0(if (!backtransf) "log-", "Abilities"), NA),
                          label.left = "Favors average treatment",
                          label.right = "Favors treatment",
                          header.line = TRUE,
                          ...) {
  
  chkclass(x, "mtrank")
  
  dat <- x$estimates
  reference.group <- x$reference.group
  #
  sorting <- setchar(sorting, c("ability", "se", "none"))
  chklogical(backtransf)
  #
  null.xlab <- is.null(xlab)
  
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
  if (null.xlab)
    xlab <- "Ability [95% CI]"
  
  if (is.null(label.left)) {
    if (x$x$small.values == "undesirable")
      label.left <- "Favours "
        
  }
  
    
  # Fit a pairwise meta-analysis model
  #
  m <- metagen(dat$log_ability, dat$se, studlab = dat$treatment,
               backtransf = backtransf, func.backtransf = "exp",
               overall = FALSE, overall.hetstat = FALSE,
               common = FALSE, random = FALSE,
               method.tau = "DL", method.tau.ci = "")
  #
  if (null.xlab & !backtransf)
    xlab <- paste0("Log-", xlab)
  
  # Create the forest plot   
  #
  forest(m,
         leftcols = leftcols, leftlabs = leftlabs,
         rightcols = rightcols, rightlabs = rightlabs,
         label.left = label.left, label.right = label.right,
         weight.study = "same", header.line = header.line,
         xlab = xlab, ...)
  
  invisible(NULL)
}
