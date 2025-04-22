#' Forest plot showing study-specific preferences or ties according to
#' treatment choice criterion
#' 
#' @description
#' This function produces a forest plot for all (or selected) study
#' specific comparisons and visualizes the treatment preference or ties
#' which are defined from the treatment choice criterion in \code{\link{tcc}}.
#' 
#' @param x An object of class \code{\link{tcc}}.
#' @param reference.group Reference treatment(s); by default all treatments are
#'   considered.
#' @param baseline.reference A logical indicating whether results
#'   should be expressed as comparisons of other treatments versus the
#'   reference treatment (default) or vice versa.
#' @param backtransf A logical indicating whether results should be
#'   back transformed. If \code{backtransf = TRUE} (default), results for
#'   \code{sm = "OR"} are printed as odds ratios rather than log odds ratios,
#'   for example.
#' @param leftcols A character vector specifying columns
#'   to be printed on the left side of the forest plot
#'   (see \code{\link[meta]{forest.meta}}).
#' @param rightcols A character vector specifying columns
#'   to be printed on the right side of the forest plot
#'   (see \code{\link[meta]{forest.meta}}).
#' @param leftlabs A character vector specifying labels for
#'   columns on left side of the forest plot.
#' @param col.winner Colour to highlight results for TCC winner.
#' @param col.tie Colour to highlight results for TCC ties.
#' @param lty.equi Line type (limits of equivalence).
#' @param col.equi Line colour (limits of equivalence).
#' @param fill.equi Colour(s) for area between limits of equivalence.
#' @param fill.mcid.below.null Colour of area below lower MCID limit.
#' @param fill.mcid.above.null Colour of area above upper MCID limit.
#' @param header.line A logical value indicating whether to print a
#'   header line or a character string ("both", "below", "").
#' @param \dots Additional arguments (passed on to
#'   \code{\link[meta]{forest.meta}}).
#' 
#' @details
#' This function produces forest plots for the study specific treatment effects
#' in the network. The color indicates whether treatment effects show
#' a preference (red color) or tie (black color). Additionally, the respective
#' range of equivalence defined at the function
#' \code{\link{tcc}} is visualized for the forest plot.
#' 
#' Argument \code{reference.group} is optional. By default, all treatments in
#' the network are considered.
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
#' 
#' @examples
#' # Examples: example(tcc)
#' 
#' @method forest tcc
#' @export

forest.tcc <- function(x,
                       reference.group = x$trts,
                       baseline.reference = x$baseline.reference,
                       backtransf = FALSE,
                       #
                       leftcols = "studlab", leftlabs = "Comparison",
                       rightcols = c("effect", "ci"),
                       #
                       col.winner = "red", col.tie = "black",
                       #
                       lty.equi = gs("lty.cid"),
                       col.equi = gs("col.cid"),
                       fill.equi = gs("fill.equi"),
                       fill.mcid.below.null = "transparent",
                       fill.mcid.above.null = "transparent",
                       #
                       header.line = TRUE,
                       ...) {
  
  #
  #
  # (1) Check arguments
  #
  #
  
  chkclass(x, "tcc")
  #
  if (!missing(reference.group))
    reference.group <-
      catch("reference.group", match.call(), x, sys.frame(sys.parent()))
  #
  if (is.null(reference.group) || all(reference.group == ""))
    reference.group <- x$trts[1]
  #
  reference.group <- unique(setchar(reference.group, x$trts))
  #
  chklogical(baseline.reference)
  chklogical(backtransf)
  #
  chkcolor(col.winner, length = 1)
  chkcolor(col.tie, length = 1)
  #
  chknumeric(lty.equi, min = 0, length = 1)
  chkcolor(col.equi, length = 1)
  #
  chklogical(header.line)
  
  
  mcid.below.null <- x$mcid.below.null
  mcid.above.null <- x$mcid.above.null
  #
  if (is_relative_effect(x$sm) & !backtransf) {
    mcid.below.null <- log(mcid.below.null)
    mcid.above.null <- log(mcid.above.null)
  }
  
  # Get rid of warning "no visible binding for global variable"
  #
  comparison <- treat1 <- treat2 <- TE <- seTE <- NULL
  #
  ppdata <- x$ppdata
  #
  dat <- NULL
  #
  for (i in seq_along(reference.group)) {
    dat.i <- ppdata %>%
      filter(treat1 == reference.group[i] | treat2 == reference.group[i])
    #
    wo <- baseline.reference & dat.i$treat1 == reference.group[i] |
      !baseline.reference & dat.i$treat2 == reference.group[i]
    #
    if (any(wo)) {
      dat.i$TE[wo] <- -dat.i$TE[wo]
      #
      ttreat1 <- dat.i$treat1[wo]
      dat.i$treat1[wo] <- dat.i$treat2[wo]
      dat.i$treat2[wo] <- ttreat1
    }
    #
    dat.i$comparison <- reference.group[i]
    #
    if (baseline.reference) {
      dat.i$comparison <- paste0("Other vs '", dat.i$comparison, "'")
      dat.i$labels <- dat.i$treat1
    }
    else {
      dat.i$comparison <- paste0("'", dat.i$comparison, "' vs other")
      dat.i$labels <- dat.i$treat2
    }
    #
    dat <- rbind(dat, dat.i)
  }
  #
  dat$color <- ifelse(dat$outcome == "winner", col.winner, col.tie)
  #
  dat <- dat %>% arrange(comparison, treat1, treat2)
  
  
  m <- suppressWarnings(metagen(TE, seTE, data = dat, sm = x$sm,
                                studlab = labels, backtransf = backtransf,
                                subgroup = dat$comparison,
                                print.subgroup.name = FALSE,
                                method.tau = "DL", method.tau.ci = "",
                                warn = FALSE))
  #
  dots_list <- drop_from_dots(list(...),
                              c("lty.cid", "col.cid",
                                "cid.below.null", "cid.above.null",
                                "fill.cid.below.null", "fill.cid.above.null",
                                "weight.study", "col.study", 
                                "col.square", "col.square.lines",
                                "calcwidth.subgroup",
                                "common", "random", "hetstat",
                                "overall", "overall.hetstat"),
                              c("lty.equi", "col.equi",
                                "mcid.below.null", "mcid.above.null",
                                "fill.mcid.below.null", "fill.mcid.above.null",
                                "", "",
                                "", "",
                                "",
                                "", "", "",
                                "", ""))
  #
  args_list <-
    list(x = m,
         header.line = header.line,
         leftcols = leftcols, leftlabs = leftlabs,
         rightcols = rightcols,
         #
         lty.cid = lty.equi, col.cid = col.equi,
         fill.cid.below.null = fill.mcid.below.null,
         fill.cid.above.null = fill.mcid.above.null,
         #
         fill.equi = fill.equi,
         cid.below.null = mcid.below.null,
         cid.above.null = mcid.above.null,
         #
         weight.study = "same",
         col.study = dat$color,
         col.square = dat$color,
         col.square.lines = dat$color,
         #
         calcwidth.subgroup = TRUE,
         #
         common = FALSE, random = FALSE, hetstat = FALSE,
         overall = FALSE, overall.hetstat = FALSE)
  #
  res <- do.call("forest", c(args_list, dots_list))
  #
  invisible(res)
}
