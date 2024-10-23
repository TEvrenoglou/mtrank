#' Forest plot of study-specific preferences or ties for treatment choice
#' criterion
#' 
#' @description
#' This function produces a forest plot for all (or some specific) study
#' specific comparisons and visualizes the treatment preference or ties
#' which are defined from the treatment choice criterion in \code{\link{tcc}}.
#' 
#' @param x An object of class \code{\link{tcc}}.
#' @param treat A treatment of interest. If specified it returns a forest plot
#' for all study specific effects related to \code{treat}. If NULL (default)
#' it generates a forest plot for all study-specific effects in the network. 
#' @param backtransf A logical indicating whether results should be
#'   back transformed. If \code{backtransf =
#'   TRUE} (default), results for \code{sm = "OR"} are printed as odds
#'   ratios rather than log odds ratios, for example.
#' @param leftcols A character vector specifying columns
#'   to be printed on the left side of the forest plot
#'   (see \code{\link[meta]{forest.meta}}).
#' @param rightcols A character vector specifying columns
#'   to be printed on the right side of the forest plot
#'   (see \code{\link[meta]{forest.meta}}).
#' @param leftlabs A character vector specifying labels for
#'   columns on left side of the forest plot.
#' @param lty.equi Line type (limits of equivalence).
#' @param col.equi Line colour (limits of equivalence).
#' @param fill.equi Colour(s) for area between limits of equivalence
#'   or more general limits.
#' @param fill.lower.equi Colour of area between lower limit(s) and
#'   reference value. Can be equal to the number of lower limits or
#'   the number of limits plus 1 (in this case the the region between
#'   minimum and smallest limit is also filled).
#' @param fill.upper.equi Colour of area between reference value and
#'   upper limit(s). Can be equal to the number of upper limits or the
#'   number of limits plus 1 (in this case the region between largest
#'   limit and maximum is also filled).
#' @param header.line A logical value indicating whether to print a
#'   header line or a character string ("both", "below", "").
#' @param col.subgroup The colour to print information on subgroups, i.e.,
#'   pairwise comparisons.
#' @param \dots Additional arguments (passed on to
#'   \code{\link[meta]{forest.meta}}).
#' 
#' @details  
#' This function produces forest plots for the study specific treatment effects
#' in the network. The legend of these graphs specifies which treatment effects
#' were identified as treatment preferences and which as treatment ties.
#' Additionally, the respective range of equivalence defined at the function
#' \code{\link{tcc}} is also visualized for each forest plot. The argument
#' \code{treat} is optional. If specified it returns only the forest plots in
#' terms of the study-specific treatment effects related to the specified
#' \code{treat}. If NULL it will return the forest plots in terms of all
#' study-specific treatment effects in the network. We recommend that the
#' specification of the argument \code{treat} especially for busy networks
#' which contain many direct comparisons. 
#' 
#' @return
#' A forest plot that shows the study-specific treatment effects and the range
#' of equivalence defined from the function \code{\link{tcc}}. Coloured
#' with red are the treatment effects which according to the \code{\link{tcc}}
#' function were producing a treatment preference and coloured with
#' black those treatment effects producing a treatment tie.
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#'
#' @examples
#' data(diabetes)
#' #
#' ranks <- tcc(treat = t, studlab = study, event = r, n = n, data = diabetes,
#'   mcid = 1.20, sm = "OR", small.values = "desirable")
#' #
#' forest(ranks)
#' forest(ranks, treat = "ARB")
#' 
#' @method forest tcc
#' @export

forest.tcc <- function(x, treat = NULL, backtransf = FALSE,
                       #
                       leftcols = "studlab", leftlabs = NULL,
                       rightcols = c("effect", "ci"),
                       #
                       lty.equi = gs("lty.equi"),
                       col.equi = gs("col.equi"),
                       fill.equi = gs("fill.equi"),
                       fill.lower.equi = fill.equi,
                       fill.upper.equi = rev(fill.equi),
                       #
                       header.line = TRUE, col.subgroup = "black",
                       ...) {
  
  chkclass(x, "tcc")
  
  lower.equi <- x$lower.equi
  upper.equi <- x$upper.equi
  #
  if (is_relative_effect(x$sm) & !backtransf) {
    lower.equi <- log(lower.equi)
    upper.equi <- log(upper.equi)
  }
    
  # Get rid of warning "no visible binding for global variable"
  #
  comparison <- treat1 <- treat2 <- NULL
  #
  dat <- x$data
  #
  dat$comparison <- paste(dat$treat1, dat$treat2, sep = " vs ")
  #
  treat <- setchar(treat, x$trts)
  #
  if (!is.null(treat))
    dat <- dat %>% filter(treat1 == treat | treat2 == treat)
  #
  dat$outcome <-
    ifelse(dat$rank1 + dat$rank2 == 2, "tie", "win") 
  #
  dat$color <- ifelse(dat$outcome == "win", "red", "black")
  #
  dat <- dat %>% arrange(comparison)
  
  m <- metagen(dat$TE, dat$seTE, sm = x$sm,
               backtransf = backtransf,
               subgroup =
                 if (length(unique(dat$comparison)) == 1)
                 NULL else dat$comparison,
               print.subgroup.name = FALSE,
               studlab = dat$studlab,
               common = FALSE, random = FALSE, hetstat = FALSE,
               method.tau = "DL", method.tau.ci = "")
  #
  if (is.null(leftlabs)) {
    if (is.null(m$subgroup))
      "Study"
    else
      "Comparison / Study"
  }
  #
  forest(m,
         backtransf = backtransf,
         header.line = header.line,
         col.subgroup = col.subgroup,
         leftcols = leftcols, leftlabs = leftlabs,
         rightcols = rightcols,
         lty.equi = lty.equi, col.equi = col.equi,
         fill.equi = fill.equi,
         fill.lower.equi = fill.lower.equi,
         fill.upper.equi = fill.upper.equi,
         #
         lower.equi = lower.equi, upper.equi = upper.equi,
         weight.study = "same",
         col.study = dat$color,
         col.square = dat$color,
         col.square.lines = dat$color,
         #
         calcwidth.subgroup = TRUE,
         #
         ...)
 
 invisible(NULL)
}
