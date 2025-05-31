#' Apply a treatment-choice criterion (TCC) to get treatment preferences 
#' based on network meta-analysis estimates.
#' 
#' @description
#' This function uses a treatment choice criterion defined by the user and
#' transforms the network meta-analysis estimates into a preference format that
#' indicates either a treatment preference or a tie. In this setting, a treatment preference 
#' implies that the respective NMA estimate represents
#' a clinically important result (i.e. that fulfills the TCC) while a tie indicates that the respective NMA estimate lacks 
#' enough evidence to represent a treatment preference. The resulting preference format is then used as input 
#' to \code{\link{mtrank}}.
#' 
#' @param x A \code{\link[netmeta]{netmeta}} object.
#' @param pooled A character string indicating whether results for the
#'   common (\code{"common"}) or random effects model
#'   (\code{"random"}) should be used. Can be abbreviated. If not specified the results from
#'   the random effects model will be used by default.
#' @param swd A numeric value specifying the smallest worthwhile difference
#'   value (swd); see Details.
#' @param swd.below.null A numeric value specifying the swd below the null
#'   effect (see Details).
#' @param swd.above.null A numeric value specifying the swd above the null
#'   effect (see Details).
#' @param small.values A character string specifying whether small 
#'   treatment effects indicate a beneficial (\code{"desirable"}) or
#'   harmful (\code{"undesirable"}) effect.
#' @param relax A logical optional argument. If TRUE (default), the treatment
#'   choice criterion is based solely on the swd bounds, emphasizing only the
#'   clinical importance of the results. If set to FALSE, the criterion
#'   incorporates both statistical significance and clinical importance.
#'   We recommend using the default setting (see Details).
#' @param level The level used to calculate confidence intervals for
#'   log-abilities.
#' @param \dots Additional arguments (ignored).
#' 
#' @details
#' R function \code{\link{mtrank}} expects data in a \bold{preference}
#' format, where a treatment preference or tie is indicated for each network
#' meta-analysis (NMA) estimate. For example, for the comparison between
#' treatments \emph{A} and \emph{B} the potential outcomes are:
#' \itemize{
#' \item \emph{A} > \emph{B}
#' \item \emph{A} < \emph{B}
#' \item \emph{A} = \emph{B}
#' }
#' 
#' The transformation takes place based on the NMA estimates and the treatment
#' choice criterion which has the form of a decision rule.
#' 
#' This function implements treatment choice criteria based on the range of
#' equivalence (ROE) which are specified by 
#' \itemize{
#' \item argument \code{swd}. Then the limits of the ROE
#'   will be defined based on the values (i) \code{swd}, \code{1 / swd} for
#'   ratio measures and (ii) \code{swd} and \code{-swd} for difference
#'   measures.
#' \item arguments \code{swd.below.null} and \code{swd.above.null}.
#'   These arguments allow the users to define their own limits of the ROE,
#'   given the restriction that the lower limit will always be smaller than the
#'   upper limit.
#' }
#' 
#' Note that when the argument \code{swd} is specified, the arguments
#' \code{swd.below.null} and \code{swd.above.null} are ignored.
#' Either only the \code{swd} or both of the \code{swd.below.null} and
#' \code{swd.above.null} must be specified for the proper
#' definition of the ROE.
#' 
#' After setting the ROE, each NMA treatment effect will be categorised as a
#' treatment preference or a tie. The argument \code{relax} controls the amount
#' of conservatism of the treatment choice criterion. If set to \code{FALSE},
#' a TCC will be built requiring both clinical importance as statistical significance of
#' the results. If set to \code{TRUE} (default), the criterion uses only the ROE bounds 
#' and therefore the NMA treatment effects need to be only clinically important to indicate a 
#' treatment preference.
#' 
#' @return
#' NMA estimates in a preference format.
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#'
#' @examples
#' data("antidepressants")
#' #
#' pw1 <- pairwise(studlab = studyid, treat = drug_name,
#'   n = ntotal, event = responders,
#'   data = antidepressants, sm = "OR")
#' # Use subset to reduce runtime
#' pw0 <- subset(pw1, studyid < 60)
#' #
#' net0 <- netmeta(pw0, reference.group = "tra")
#' 
#' ranks0 <- tcc(net0, swd = 1.20, small.values = "undesirable")
#' 
#' # Comparison other drugs vs trazodone
#' forest(ranks0,
#'   label.left = "Favours trazodone",
#'   label.right = "Favours other drug")
#' 
#' # Comparison escitalopram vs other drugs
#' forest(ranks0, reference.group = "esc", baseline = FALSE,
#'   label.left = "Favours other drug",
#'   label.right = "Favours escitalopram")
#' 
#' \donttest{
#' # Store a PDF file in the current working directory showing all results
#' # (this is the default, i.e., if argument 'reference.group' is missing)
#' forest(ranks0, baseline = FALSE, reference.group = trts,
#'   file = "forest_tcc_antidepressants.pdf")
#' }
#' 
#' \dontrun{
#' # Run analysis with full data set
#' net1 <- netmeta(pw1, reference.group = "tra")
#' 
#' ranks1 <- tcc(net1, swd = 1.20, small.values = "undesirable")
#' 
#' # Comparison other drugs vs trazodone
#' forest(ranks1,
#'   label.left = "Favours trazodone",
#'   label.right = "Favours other drug")
#' 
#' # Comparison escitalopram vs other drugs
#' forest(ranks1, reference.group = "esc", baseline = FALSE,
#'   label.left = "Favours other drug",
#'   label.right = "Favours escitalopram")
#' }
#'
#' @export tcc

tcc <- function(x,
                pooled = if (x$random) "random" else "common",
                swd = NULL, swd.below.null = NULL, swd.above.null = NULL,
                small.values = x$small.values,
                relax = TRUE, level = x$level.ma) {
  
  #
  #
  # (1) Check arguments
  #
  #
  
  chkclass(x, "netmeta")
  #
  sm <- x$sm
  is_relative <- is_relative_effect(sm)
  #
  pooled <- setchar(pooled, c("common","random","fixed"))
  pooled[pooled == "fixed"] <- "common"
  #
  small.values <- setsv(small.values)
  #
  chklogical(relax)
  chklevel(level)
  #
  if (!is.null(swd)) {
    if (is_relative) {
      if (swd == 1) {
        swd.below.null <- swd.above.null <- 1
        warning("A minimal clinically important difference equal to 1 results ", 
                "in a range of equivalence (ROE) with both bounds equal to 1.")
      }
      else if (swd == 0) {
        swd.below.null <- swd.above.null <- 0
        warning("A minimal clinically important difference equal to 0 results ", 
                "in a range of equivalence (ROE) with both bounds equal to 0.")
      }
      else {
        swd.below.null <- min(swd, 1/swd)
        swd.above.null <- max(swd, 1/swd)
      }
    }
    else {
      swd.below.null <- min(swd, -swd)
      swd.above.null <- max(swd, -swd)
    }
  }
  else if (is.null(swd.below.null) & is.null(swd.above.null))
    stop("Either argument 'swd' or arguments 'swd.below.null' and ",
         "'swd.above.null') must be specified.",
         call. = FALSE)
  #
  if (swd.below.null > swd.above.null)
    stop("Input for argument 'swd.below.null' must be smaller than ",
         "'swd.above.null'.",
         call. = FALSE)
  #
  if (is_relative) {
    swd.below.null <- log(swd.below.null)
    swd.above.null <- log(swd.above.null)
  }
  #
  if (relax) {
    no_effect1 <- swd.below.null
    no_effect2 <- swd.above.null
  }
  else {
    no_effect1 <- 0
    no_effect2 <- 0
  }
  
  
  #
  #
  # (2) Generate data set in paired-preference format
  #
  #
  
  pdat <- net2dat(x, pooled)
  #
  pdat$.order <- seq_len(nrow(pdat))
  #
  ci.p <- ci(pdat$TE, pdat$seTE, level = level)
  #
  pdat$lower <- ci.p$lower
  pdat$upper <- ci.p$upper
  #
  pdat$rank_text <- ""
  pdat$rank1 <- NA
  pdat$rank2 <- NA
  #
  if (small.values == "undesirable") {
    sel1 <- pdat$upper < swd.below.null
    sel2 <- pdat$lower < swd.below.null & pdat$TE < swd.below.null & 
      pdat$upper < no_effect2
    #
    pdat$rank_text[sel1 | sel2] <-
      paste(pdat$treat2[sel1 | sel2], pdat$treat1[sel1 | sel2], sep = " > ")
    #
    pdat$rank1[sel1 | sel2] <- 2
    pdat$rank2[sel1 | sel2] <- 1
    #
    sel3 <- pdat$lower > swd.above.null
    sel4 <- pdat$upper > swd.above.null & pdat$TE > swd.above.null & 
      pdat$lower > no_effect1
    #
    pdat$rank_text[sel3 | sel4] <-
      paste(pdat$treat1[sel3 | sel4], pdat$treat2[sel3 | sel4], sep = " > ")
    #
    pdat$rank1[sel3 | sel4] <- 1
    pdat$rank2[sel3 | sel4] <- 2
    #
    sel5 <- !(sel1 | sel2 | sel3 | sel4)
    #
    pdat$rank_text[sel5] <-
      paste(pdat$treat1[sel5], pdat$treat2[sel5], sep = " = ")
    #
    pdat$rank1[sel5] <- 1
    pdat$rank2[sel5] <- 1
  }
  else {
    sel1 <- pdat$upper < swd.below.null
    sel2 <- pdat$lower < swd.below.null & pdat$TE < swd.below.null & 
      pdat$upper < no_effect2
    #
    pdat$rank_text[sel1 | sel2] <-
      paste(pdat$treat1[sel1 | sel2], pdat$treat2[sel1 | sel2], sep = " > ")
    #
    pdat$rank1[sel1 | sel2] <- 1
    pdat$rank2[sel1 | sel2] <- 2
    #
    sel3 <- pdat$lower > swd.above.null
    sel4 <- pdat$upper > swd.above.null & pdat$TE > swd.above.null & 
      pdat$lower > no_effect1
    #
    pdat$rank_text[sel3 | sel4] <-
      paste(pdat$treat2[sel3 | sel4], pdat$treat1[sel3 | sel4], sep = " > ")
    #
    pdat$rank1[sel3 | sel4] <- 2
    pdat$rank2[sel3 | sel4] <- 1
    #
    sel5 <- !(sel1 | sel2 | sel3 | sel4)
    #
    pdat$rank_text[sel5] <-
      paste(pdat$treat1[sel5], pdat$treat2[sel5], sep = " = ")
    #
    pdat$rank1[sel5] <- 1
    pdat$rank2[sel5] <- 1
  }
  #
  # Get rid of warning "no visible binding for global variable"
  #
  rank1 <- rank2 <- id <- outcome <- treat1 <- treat2 <- treat <-
    grp <- .order <- NULL
  #
  pdat %<>% mutate(outcome = if_else(rank1 + rank2 != 2, "winner", "tie"))
  #
  with.tie <- unique(pdat$id[pdat$outcome == "tie"])
  #
  pdat1 <- subset(pdat, id %in% with.tie) %>%
    arrange(id, outcome, treat1, treat2)
  pdat2 <- subset(pdat, !(id %in% with.tie)) %>%
    arrange(id, outcome, treat1, treat2)
  #
  pdat <- rbind(pdat1, pdat2) %>%
    mutate(id = seq_len(nrow(pdat1) + nrow(pdat2))) %>%
    relocate(.order, .after = last_col())
  #
  class(pdat) <- c("ppdata", class(pdat))
  #
  # Data set in long format
  #
  ldat1 <- pdat %>%
    mutate(treat = treat1, rank = rank1, grp = 1) %>% 
    select(id, grp, id, treat, rank, outcome)
  #
  ldat2 <- pdat %>%
    mutate(treat = treat2, rank = rank2, grp = 2) %>% 
    select(id, grp, id, treat, rank, outcome)
  #
  ldat <- rbind(ldat1, ldat2) %>% arrange(id, grp)
  #
  class(ldat) <- "data.frame"
  #
  preferences <- rankings(ldat, id = "id", item = "treat", rank = "rank")
  #
  all.ties <- all(pdat$outcome == "tie")
  #
  if (all.ties) 
    warning("Only ties were identified through the treatment choice ", 
            "criterion. This can yield into convergence problems when ", 
            "using mtrank().")
  #
  if (is_relative) {
    swd.below.null <- exp(swd.below.null)
    swd.above.null <- exp(swd.above.null)
  }
  
  
  #
  #
  # (3) Return results
  #
  #
  
  res <- list(ppdata = pdat, preferences = preferences, 
              small.values = small.values, 
              swd = swd,
              swd.below.null = swd.below.null,
              swd.above.null = swd.above.null, 
              no_effect1 = no_effect1,
              no_effect2 = no_effect2,
              all.ties = all.ties, 
              sm = sm, level = level,
              trts = sort(unique(c(pdat$treat1, pdat$treat2))),
              reference.group = x$reference.group,
              baseline.reference = x$baseline.reference)
  #
  class(res) <- c("tcc", class(res))
  #
  attr(res,"net.obj") <- x
  #
  res
}

#' @rdname tcc
#' @method print tcc
#' @export

print.tcc <- function(x, ...) {
  
  chkclass(x, "tcc")

  print(x$preferences)
  #
  invisible(NULL)
}
