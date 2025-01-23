#' Transform meta-analysis data from long or wide arm-based format into
#' paired-preference format
#' 
#' @description
#' This function transforms data that are given in wide or long
#' arm-based format (e.g. input format for WinBUGS or JAGS) to a
#' paired-preference format needed as input to \code{\link{mtrank}}.
#' The function can transform data with binary and continuous arm-based to
#' preference-based format.
#' 
#' @param treat Either a \code{\link[meta]{pairwise}} object, or a list or
#'   vector with treatment information for individual treatment arms
#'   (see Details).
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
#' @param mcid A numeric vector specifying the minimal clinically important
#'   value (see Details). 
#' @param lower.equi A numeric value specifying the lower limit of the range of
#'   equivalence (see Details).
#' @param upper.equi A numeric value specifying the upper limit of the range of
#'   equivalence (see Details).
#' @param small.values A character string specifying whether small 
#' treatment effects indicate a beneficial (\code{"desirable"}) or
#' harmful (\code{"undesirable"}) effect.
#' @param relax A logical optional argument. If TRUE it 'relaxes' the tcc 
#'   to only consider the bounds of ROE when specifying 'wins' and ties. 
#'   The default FALSE uses the criterion described by Evrenoglou et al. (2024)
#'   and considers also the statistical significance on top of the ROE bounds
#'   (see Details).
#' @param level The level used to calculate confidence intervals for
#'   log-abilities.
#' @param sm The effect measure of interest (see Details).
#' @param keepdata A logical indicating whether original data should be kept in
#'   tcc object.
#' @param x An object of class \code{\link{tcc}}.
#' @param \dots Additional arguments (passed on to
#'   \code{\link[meta]{pairwise}}).
#' 
#' @details
#' R function \code{\link{mtrank}} expects data in a \bold{paired-preference}
#' format, where for each study-specific pairwise comparison in the network a
#' treatment preference or tie is indicated. For example, for the
#' study-specific comparison between treatments \emph{A} and \emph{B} the
#' potential outcomes are:
#' \itemize{
#' \item \emph{A} > \emph{B}
#' \item \emph{A} < \emph{B}
#' \item \emph{A} = \emph{B}
#' }
#' 
#' The data transformation takes place based on the study-specific treatment
#' effects and the treatment choice criterion. R function
#' \code{\link[meta]{pairwise}} is called internally to calculate the
#' study-specific treatment effect estimates and standard errors. This ensures
#' that data given in either 'long' or 'wide' arm-based format will be suitably
#' used to calculate the study-specific treatment effect estimates and standard
#' errors while ensuring that a network of multi-arm studies gets an
#' equivalent representation as a network of two-arm studies. It is also
#' possible to provide a \code{\link[meta]{pairwise}} as the main input.
#' In this case, inputs for the arguments \code{event}, \code{n}, \code{mean},
#' \code{sd}, \code{data}, \code{studlab}, or \code{keepdata} are ignored.
#' 
#' This function implements treatment choice criteria based on the method
#' by Evrenoglou et al. (2024). Namely, a range of equivalence (ROE) can be
#' specified by
#' \itemize{
#' \item argument \code{mcid}. Then the limits of the ROE
#'   will be defined based on the values (i) \code{mcid}, \code{1/mcid} for
#'   ratio measures and (ii) \code{mcid} and \code{-mcid} for difference
#'   measures.
#' \item arguments \code{lower.equi} and \code{upper.equi}.
#'   These arguments allow the users to define their own limits of the ROE,
#'   given the restriction that the lower limit will always be smaller than the
#'   upper limit.
#' }
#' 
#' Note that when the argument \code{mcid} is specified, the arguments
#' \code{lower.equi} and \code{upper.equi} are ignored.
#' Either only the \code{mcid} or both of the \code{lower.equi} and
#' \code{upper.equi} must be specified for the proper
#' definition of the ROE.
#' 
#' After setting the ROE, each study-specific treatment effect will be
#' categorised as a treatment preference or a tie. The argument \code{relax}
#' controls the amount of conservatism of the treatment choice criterion.
#' If set to \code{FALSE} (default), the treatment choice criterion is
#' equivalent to the one described by Evrenoglou et al. (2024). In this case,
#' study-specific treatment effects need to be both statistically and clinically
#' significant to indicate a treatment preference. If set to \code{TRUE}, the
#' criterion is relaxed and the study-specific treatment effects need to be only
#' clinically significant to indicate a treatment preference. 
#' 
#' This function can transform data with binary and continuous outcomes.
#' Depending on the outcome, the following arguments are mandatory:
#' \itemize{
#' \item treat, event, n (for binary outcomes);
#' \item treat, n, mean, sd (for continuous outcomes).
#' }
#' 
#' Finally, the argument \code{sm} is used to define the effect measure of
#' interest for transforming the data into paired-preference format;
#' see \code{\link[meta]{metabin}} and \code{\link[meta]{metacont}} for a
#' list of available effect measures.
#' 
#' @return
#' \itemize{
#' \item The initial data in a paired-preference format.
#' \item The correspondence between the initial study names
#'   (passed in the argument studlab) and the index name of the
#'   paired-preference format data.
#' }
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
#' forest(ranks, treat = "ARB")
#'
#' @export tcc

tcc <- function(treat,
                event, n, mean, sd,
                data = NULL, studlab,
                mcid = NULL, lower.equi = NULL, upper.equi = NULL,
                small.values = gs("small.values"),
                relax = FALSE, level = 0.95, sm,
                keepdata = gs("keepdata"),
                ...) {
  
  missing.event <- missing(event)
  missing.n <- missing(n)
  missing.mean <- missing(mean)
  missing.sd <- missing(sd)
  missing.studlab <- missing(studlab)
  missing.sm <- missing(sm)
  missing.keepdata <- missing(keepdata)
  
  
  # Check additional arguments
  #
  if (missing(mcid)) {
    if (missing(lower.equi) | missing(upper.equi))
      stop("A range of equivalence (ROE) needs to be defined. ",
           "For a symmetrical ROE you can specify it using the ",
           "argument 'mcid' or you can explicitly define the ROE ",
           "lower and upper limits using the arguments 'lower.equi' ",
           "and 'upper.equi'.")
    else {
      chknumeric(lower.equi)
      chknumeric(upper.equi)
      #
      if ((lower.equi > upper.equi) || (lower.equi == upper.equi)) {
        stop("Argument 'lower.equi' must be smaller than 'upper.equi.'")
      }
    }
  }
  else {
    chknumeric(mcid)
    #
    if (!missing(lower.equi))
      warning("Argument 'lower.equi' ignored as argument 'mcid' was provided.")
    #
    if (!missing(upper.equi))
      warning("Argument 'upper.equi' ignored as argument 'mcid' was provided.")
  }
  #
  small.values <- setchar(small.values, c("desirable", "undesirable"))
  #
  chklogical(relax)
  #
  chklevel(level)
  #
  chklogical(keepdata)
  
  
  # Get rid of warning 'no visible binding for global variable'
  #
  grp <- id <- outcome <- rank1 <- rank2 <- treat1 <- treat2 <-
    .order <- NULL
  
  #
  # Read the data from a data frame
  #
  
  nulldata <- is.null(data)
  sfsp <- sys.frame(sys.parent())
  mc <- match.call()
  #
  if (nulldata)
    data <- sfsp
  #
  treat <- catch("treat", mc, data, sfsp)
  #
  if (inherits(treat, "pairwise")) {
   pdat <- treat
   #
   txt.ignore <- "as first argument is a pairwise object"
   #
   ignore_input(event, !missing.event, txt.ignore)
   ignore_input(n, !missing.n, txt.ignore)
   ignore_input(mean, !missing.mean, txt.ignore)
   ignore_input(sd, !missing.sd, txt.ignore)
   ignore_input(data, !nulldata, txt.ignore)   
   ignore_input(studlab, !missing.studlab, txt.ignore)
   ignore_input(sm, !missing.sm, txt.ignore)
   ignore_input(keepdata, !missing.keepdata, txt.ignore)
   #
   sm <- attr(treat, "sm")
   keepdata <- FALSE
  }
  else {
    event <- catch("event", mc, data, sfsp)
    mean <- catch("mean", mc, data, sfsp)
    sd <- catch("sd", mc, data, sfsp)
    n <- catch("n", mc, data, sfsp)
    studlab <- catch("studlab", mc, data, sfsp)
    #
    chknull(studlab)
    chknull(treat)
    #
    if (is.list(treat))
      chklist(treat)
    #
    if (!is.null(event))
      if (is.list(event))
        chklist(event)
    else
      chknumeric(event)
    #
    if (!is.null(n))
      if (is.list(n))
        chklist(n)
    else
      chknumeric(n)
    #
    if (!is.null(mean))
      if (is.list(mean))
        chklist(mean)
    else
      chknumeric(mean)
    #
    if (!is.null(sd))
      if (is.list(sd))
        chklist(sd)
    else
      chknumeric(sd)
        
    
    #
    #
    # Determine whether the outcome is binary or continuous
    #
    #
    
    if (!is.null(event) & !is.null(n) & is.null(mean) & is.null(sd))
      type <- "binary"
    else if (!is.null(n) & !is.null(mean) & !is.null(sd))
      type <- "continuous"
    else
      stop("Type of outcome unclear. Please provide the necessary ",
           "information:\n  - event, n (binary outcome)\n  - n, ",
           "mean, sd (continuous outcome).")
    #
    if (type == "binary")
      sm <- setchar(sm, gs("sm4bin"))
    else
      sm <- setchar(sm, gs("sm4cont"))
    
    
    #
    #
    # Determine the format of the data
    #
    #
    
    data.format <- NA
    #
    if (type == "binary") {
      if (is.list(event) & is.list(n) & is.list(treat)) {
        if ((length(event) != length(n)) ||
            (length(event) != length(treat)) ||
            (length(treat) != length(n)))
          stop("List arguments 'treat', 'event' and 'n' must have ",
               "the same length.")
        #
        if (length(event) == 1 & length(n) == 1 & length(treat) == 1) {
          data.format <- "long"
          #
          event <- unlist(event)
          n <- unlist(n)
          treat <- unlist(treat)
        }
        else if (length(event) > 1 & length(n) > 1 & length(treat) > 1)
          data.format <- "wide" ## don't unlist here
      }
      else if (is.numeric(event) & is.numeric(n))
        data.format <- "long"
    }
    else {
      if (is.list(mean) & is.list(sd) &  is.list(n) & is.list(treat)) {
        if ((length(mean) != length(n)) ||
            (length(mean) != length(treat)) ||
            (length(mean) != length(sd)) ||
            (length(n) != length(sd)) ||
            (length(n) != length(treat)) ||
            (length(sd) != length(treat)))
          stop("List arguments 'treat', 'mean', 'sd' and 'n' must have the ",
               "same length.")
        #
        if (length(mean) == 1 & length(sd) == 1 & length(n) == 1 &
            length(treat) == 1) {
          data.format <- "long"
          #
          mean <- unlist(mean)
          n <- unlist(n)
          sd <- unlist(sd)
          treat <- unlist(treat)
        }
        else if (length(mean) > 1 & length(sd) > 1 & length(n) > 1 &
                 length(treat) > 1)
          data.format <- "wide" ## don't unlist here 
      }
      else if (is.numeric(mean) & is.numeric(sd) &  is.numeric(n))
        data.format <- "long"
    }
    
    
    if (data.format == "long") {
      if (type == "binary")
        dat <- data.frame(studlab, treat, event, n)
      else
        dat <- data.frame(studlab, treat, mean, sd, n)
    }
    else if (data.format == "wide") {
      if (type == "binary")
        dat <- go_long(studlab = studlab, treat = treat, event = event,
                       n = n, type = type)
      else
        dat <- go_long(studlab = studlab, treat = treat,
                       mean = mean, sd = sd, n = n, type = type)
    }
    
    #
    # Clean studies with
    # (i) zero events in all arms or missing number of events
    #     and sample sizes (binary outcome) or
    # (ii) missing information on means or sample sizes (continuous outcome)
    #
    
    dat <- clean(dat, type)
        
    
    # Use pairwise to transform the long format data to contrasts using
    # pairwise() from netmeta.
    # This also ensures that a network of multi-arm studies gets an equivalent
    # representation of a network of two-arm studies.
    
    if (type == "binary")
      pdat <- pairwise(data = dat, treat = treat, event = event,
                       n = n, sm = sm, studlab = studlab,
                       append = FALSE, ...)
    else
      pdat <- pairwise(data = dat, treat = treat, mean = mean,
                       sd = sd, n = n, sm = sm, studlab = studlab,
                       append = FALSE, ...)
  }
  #
  # Keep original order
  #
  pdat$.order <- seq_len(nrow(pdat))
  #
  # Add confidence limits
  #
  ci.p <- ci(pdat$TE, pdat$seTE, level = level)
  #
  pdat$lower <- ci.p$lower
  pdat$upper <- ci.p$upper
  
  
  #
  # Define the Range of Equivalence based on
  # (i) the 'civ' argument which automatically produces a symmetrical ROE or
  # (ii) by specifying the bounds of the ROE based on the arguments
  #      'lower.equi' and 'upper.equi'
  #
  
  if (!is.null(mcid)) {
    if (is_relative_effect(sm)) {
      if (mcid == 1) {
        lower.equi <- upper.equi <- 1
        #
        warning("A minimal clinically important difference equal to 1 results ",
                "in a range of equivalence (ROE) with both bounds equal to 1.")
      }
      else if (mcid == 0) {
        lower.equi <- upper.equi <- 0
        #
        warning("A minimal clinically important difference equal to 0 results ",
                "in a range of equivalence (ROE) with both bounds equal to 0.")
      }
      else {
        lower.equi <- min(mcid, 1 / mcid)
        upper.equi <- max(mcid, 1 / mcid)
      }
    }
    else {
      lower.equi <- min(mcid, -mcid)
      upper.equi <- max(mcid, -mcid)
    }
  }
  
  #
  # Define the treatment choice criterion
  # For that we need:
  # (i) what is the null effect,
  # (ii) how the ROE of equivalence will be treated
  #
  
  # the argument relax defines how the ROE should be treated. The default is FALSE. If TRUE, it relaxes the TTC shown in the main manuscript.
  
  # The modified tcc does not account for the statistical significance of the effect. Now for a relative effect to indicate a "win" 
  # it requires that the relative effect (point estimate) and one bound of the confidence interval will be clinically significant (i.e. outside the ROE) 
  # and the other bound simply does not indicate a clinically significant effect on the opposite direction.
  
  if (relax) {
    no_effect1 <- lower.equi
    no_effect2 <- upper.equi
  }
  else {
    no_effect1 <- no_effect2 <- 0
  }
  
  
  #
  #
  # Define wins and ties for each study specific pairwise comparison
  #
  #
  
  if (is_relative_effect(sm)) {
    lower.equi <- log(lower.equi)
    upper.equi <- log(upper.equi)
  }
  #
  pdat$rank_text <- ""
  pdat$rank1 <- NA
  pdat$rank2 <- NA
  #
  if (small.values == "undesirable") {
    # Since 'small.values' are undesirable the larger the treatment effect
    # the more likely that for the comparison t1 vs t2, t1 is preferred.
    sel1 <- pdat$upper < lower.equi
    #
    sel2 <-
      pdat$lower < lower.equi & pdat$TE < lower.equi & pdat$upper < no_effect2
    #
    pdat$rank_text[sel1 | sel2] <-
      paste(pdat$treat2[sel1 | sel2], pdat$treat1[sel1 | sel2], sep = " > ")
    #
    pdat$rank1[sel1 | sel2] <- 2
    pdat$rank2[sel1 | sel2] <- 1
    #
    sel3 <- pdat$lower > upper.equi
    #
    sel4 <-
      pdat$upper > upper.equi & pdat$TE > upper.equi & pdat$lower > no_effect1
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
    # Since 'small.values' are desirable the smaller the treatment effect
    # the more likely that for the comparison t1 vs t2, t1 is preferred.
    sel1 <- pdat$upper < lower.equi
    #
    sel2 <-
      pdat$lower < lower.equi & pdat$TE < lower.equi & pdat$upper < no_effect2
    #
    pdat$rank_text[sel1 | sel2] <-
      paste(pdat$treat1[sel1 | sel2], pdat$treat2[sel1 | sel2], sep = " > ")
    #
    pdat$rank1[sel1 | sel2] <- 1
    pdat$rank2[sel1 | sel2] <- 2
    #
    sel3 <- pdat$lower > upper.equi
    #
    sel4 <-
      pdat$upper > upper.equi & pdat$TE > upper.equi & pdat$lower > no_effect1
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
  pdat %<>% mutate(outcome = if_else(rank1 + rank2 != 2, "winner", "tie"))
  #
  with.tie <- unique(pdat$studlab[pdat$outcome == "tie"])
  #
  p1 <- subset(pdat, studlab %in% with.tie) %>%
    arrange(studlab, outcome, treat1, treat2)
  p2 <- subset(pdat, !(studlab %in% with.tie)) %>%
    arrange(studlab, outcome, treat1, treat2)
  #
  pdat <- rbind(p1, p2) %>% mutate(id = seq_len(nrow(p1) + nrow(p2))) %>%
    relocate(.order, .after = last_col())
  #
  class(pdat) <- c("ppdata", class(pdat))
  #
  pdat1 <- pdat %>% mutate(treat = treat1, rank = rank1, grp = 1) %>%
    select(id, grp, studlab, treat, rank, outcome)
  #
  pdat2 <- pdat %>% mutate(treat = treat2, rank = rank2, grp = 2) %>%
    select(id, grp, studlab, treat, rank, outcome)
  #
  ldat <- rbind(pdat1, pdat2) %>% arrange(id, grp)
  class(ldat) <- "data.frame"
  #
  ungrouped.preferences <-
    rankings(ldat, id = "id", item = "treat", rank = "rank")
  #
  grouped.preferences <-
    as.rankings(ungrouped.preferences,
                index = as.numeric(as.factor(pdat$studlab)))
  #
  all.ties <- all(pdat$outcome == "tie")
  #
  if (all.ties)
    warning("Only ties were identified through the treatment choice ",
            "criterion. This can yield into convergence problems when ",
            "using mtrank().")
  
  
  #
  if (is_relative_effect(sm)) {
    lower.equi <- exp(lower.equi)
    upper.equi <- exp(upper.equi)
  }
  #
  res <- list(ppdata = pdat,
              #
              grouped.preferences = grouped.preferences,
              ungrouped.preferences = ungrouped.preferences,
              #
              small.values = small.values,
              mcid = mcid,
              lower.equi = lower.equi, upper.equi = upper.equi,
              no_effect1 = no_effect1, no_effect2 = no_effect2,
              all.ties = all.ties,
              #
              sm = sm, level = level,
              #
              trts = sort(unique(c(pdat$treat1, pdat$treat2))),
              #
              data = if (keepdata) data else NULL,
              #
              call = match.call(),
              version = packageVersion("mtrank"))
  #
  class(res) <- c("tcc", class(res))
  #
  res
}


#' @rdname tcc
#' @method print tcc
#' @export

print.tcc <- function(x, ...) {
  
  chkclass(x, "tcc")
  
  print(x$grouped.preferences)
  #
  invisible(NULL)
}
