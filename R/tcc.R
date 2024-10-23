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
#' @param x An object of class \code{\link{tcc}}.
#' @param \dots Additional arguments.
#' 
#' @details
#' R function \code{\link{mtrank}} expects data in a
#' \bold{paired-preference} format, where for each study-specific pairwise
#' comparison in the network a treatment preference or a treatment tie is
#' indicated. For example, for the study-specific comparison between treatments
#' \emph{A} and \emph{B} the potential outcomes are:
#' \itemize{
#' \item \emph{A} > \emph{B}
#' \item \emph{A} < \emph{B}
#' \item \emph{A} = \emph{B}
#' }
#' 
#' The data transformation takes place based on the study-specific treatment
#' effects and the treatment choice criterion. To calculate the study-specific
#' treatment effect estimates and standard errors the tcc function uses the
#' \code{\link{pairwise}} function which is available from R package
#' \bold{netmeta}. This ensures that data given in either 'long' or 'wide'
#' arm-based format will be suitably used to calculate the study-specific
#' treatment effect estimates and standard errors while ensuring that a network
#' of multi-arm studies gets and equivalent representation as a network of
#' two-arm studies.
#' 
#' The tcc function includes treatment choice criteria based on the method
#' by Evrenoglou et al. (2024). This approach uses the range of equivalence
#' (ROE) which through the \code{tcc} function can be defined in two different
#' ways:
#' \itemize{
#' \item by specifying the argument \code{mcid}. Then the limits of the ROE
#'   will be defined based on the values (i) \code{mcid}, \code{1/mcid} for
#'   ratio measures and (ii) \code{mcid} and \code{-mcid} for difference
#'   measures.
#' \item by specifying the arguments \code{lower.equi} and \code{upper.equi}.
#'   These arguments allow the users to define their own limits of the ROE,
#'   given the restriction that the lower limit will always be smaller than the
#'   upper limit.
#' }
#' 
#' Note that when the argument \code{mcid} is specified, the arguments
#' \code{lower.equi} and \code{upper.equi} are deprecated. 
#' Either only the \code{mcid} or both of the \code{lower.equi} and
#' \code{upper.equi} should be specified mandatorily specified for the proper
#' definition of the ROE.
#' 
#' After setting the ROE, each study-specific treatment effect will be
#' categorised as a treatment preference or a tie. The argument \code{relax}
#' controls the amount of conservatism of the treatment choice criterion.
#' If set as FALSE (default), the treatment choice criterion is equivalent to
#' the one described by Evrenoglou et al. (2024). In this case, study-specific
#' treatment effects need to be both statistically and clinically significant
#' to indicate a treatment preference. If set to TRUE, the criterion is relaxed
#' and the study-specific treatment effects need to be only clinically
#' significant to indicate a treatment preference. 
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
#' see \code{\link{metabin}} and \code{\link{metacont}} for a list of available
#' effect measures.
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
                small.values = "desirable",
                relax = FALSE, level = 0.95, sm,
                ...) {
  
  # Get rid of warning 'no visible binding for global variable'
  #
  grp <- id <- outcome <- rank1 <- rank2 <- treat1 <- treat2 <- NULL
  
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
  studlab <- catch("studlab", mc, data, sfsp)
  treat <- catch("treat", mc, data, sfsp)
  event <- catch("event", mc, data, sfsp)
  mean <- catch("mean", mc, data, sfsp)
  sd <- catch("sd", mc, data, sfsp)
  n <- catch("n", mc, data, sfsp)
  #  
  args <- list(...)
  nam.args <- names(args)
  
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
  chklogical(relax)
  #
  chklevel(level)
  
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
  
  ## the argument relax defines how the ROE should be treated. The default is FALSE. If TRUE, it relaxes the TTC shown in the main manuscript.
  
  ## The modified tcc does not account for the statistical significance of the effect. Now for a relative effect to indicate a "win" 
  ## it requires that the relative effect (point estimate) and one bound of the confidence interval will be clinically significant (i.e. outside the ROE) 
  ## and the other bound simply does not indicate a clinically significant effect on the opposite direction.
  
  if (relax) {
    no_effect1 <- lower.equi
    no_effect2 <- upper.equi
  }
  else {
    no_effect1 <- no_effect2 <- 0
  }
  
  
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
  #
  # Add confidence limits
  #
  ci.p <- ci(pdat$TE, pdat$seTE, level = level)
  #
  pdat$lower <- ci.p$lower
  pdat$upper <- ci.p$upper
  
  
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
  pdat <- rbind(p1, p2)
  #
  pdat$id <- seq_len(nrow(pdat))
  #
  pdat1 <- pdat %>% mutate(treat = treat1, rank = rank1, grp = 1) %>%
    select(id, grp, studlab, treat, rank, outcome)
  #
  pdat2 <- pdat %>% mutate(treat = treat2, rank = rank2, grp = 2) %>%
    select(id, grp, studlab, treat, rank, outcome)
  #
  ldat <- rbind(pdat1, pdat2) %>% arrange(id, grp)
  #
  all.ties <- all(pdat$outcome == "tie")
  #
  if (all.ties)
    warning("Only ties were identified through the treatment choice ",
            "criterion. This can yield into convergence problems when ",
            "using mtrank().")
  #
  ungrouped.preferences <-
    rankings(ldat, id = "id", item = "treat", rank = "rank")
  #
  grouped.preferences <-
    as.rankings(ungrouped.preferences,
                index = as.numeric(as.factor(pdat$studlab)))
  
  if (is_relative_effect(sm)) {
    lower.equi <- exp(lower.equi)
    upper.equi <- exp(upper.equi)
  }
  #
  res <- list(grouped.preferences = grouped.preferences,
              pdat = pdat, ldat = ldat,
              #
              data = pdat,
              #
              lower.equi = lower.equi,
              upper.equi = upper.equi,
              #
              sm = sm,
              level = level,
              #
              ungrouped.preferences = ungrouped.preferences,
              #
              mcid = mcid,
              lower.equi = lower.equi, upper.equi = upper.equi,
              no_effect1 = no_effect1, no_effect2 = no_effect2,
              all.ties = all.ties,
              small.values = small.values,
              #
              trts = sort(unique(c(pdat$treat1, pdat$treat2))),
              #
              call = match.call(),
              version = packageVersion("mtrank"))
  #
  class(res) <- c("tcc", class(res))
  #
  res
}





if (FALSE) {
    print(PlackettLuce(ungrouped.preferences2))
  
  #
  # Ensure that preferences from multi-arm studies are grouped together using
  # the grouping criterion based on "index".
  # The number of rows is equal to the number of studies in the network.
  #
  # The output of this function gives the grouped study preferences.
  # The order that the comparisons appear correspond to the
  # order of rows in the 'index_match' which is created below.
  #
  
  grouped.preferences2 <-
    as.rankings(ungrouped.preferences2, index = as.numeric(ldat$studlab))
  print(PlackettLuce(grouped.preferences2))

  
  if (is_relative_effect(sm)) {
    lower.equi <- exp(lower.equi)
    upper.equi <- exp(upper.equi)
  }
  #
  pdat$idx <- seq_len(nrow(pdat))
  #
  u <- pdat$idx
  
  r <- list()
  r1 <- list()
  x <- list()
  rank_final <- list()
  ind <- list()
  index <- list()
  
  
  #
  #
  # Define wins and ties for each study specific pairwise comparison
  #
  #
  
  for (i in seq_len(length(u))) {
    r[[i]] <- pdat %>%  # each "i" in the list r[[]] is a row of the dataset obtained from the pairwise function. 
      filter(idx == u[[i]]) # therefore the list r[[]] contains all the study-specific pairwise comparisons
    
    r1[[i]] <- r[[i]] %>% # keep only the elements required for the tcc. These are: (i) the study id, (ii) the treatment labels, (iii) the treatments effects, (iv) the treatment effect uncertainty 
      select(studlab, treat1, treat2, TE, seTE, studlab_orig)
    
    # Calculate Wald type confidence intervals for each study specific
    # pairwise comparison
    r1[[i]]$L <- r1[[i]]$TE - qnorm(0.975) * r1[[i]]$seTE
    
    r1[[i]]$U <- r1[[i]]$TE + qnorm(0.975) * r1[[i]]$seTE
    
    # For the case of a binary outcome and when the user indicates "RR" or "OR"
    # as effect measure the pairwise() function returns them in a log-scale
    # here we transform them into the natural scale for both the point estimates
    # and the confidence interval bounds. 
    # In any other case of effect measures no other transformation takes place.
    
    if (is_relative_effect(sm)) {
      r1[[i]]$es <- exp(r1[[i]]$TE)
      r1[[i]]$lower.es <- exp(r1[[i]]$L)
      r1[[i]]$upper.es <- exp(r1[[i]]$U)
    }
    else {
      r1[[i]]$es <- r1[[i]]$TE
      r1[[i]]$lower.es <- r1[[i]]$L
      r1[[i]]$upper.es <- r1[[i]]$U
    }
    
    r1[[i]]$rank_text <- NA
    
    r1[[i]]$rank1 <- NA
    
    r1[[i]]$rank2 <- NA
    
    #### Define tcc for the case where small.values are 'undesirable' and for the case that they are 'desirable'
    
    if (small.values == "undesirable") {
      # Since 'small.values' are undesirable the larger the treatment effect
      # the more likely that for the comparison t1 vs t2, t1 is preferred.
      
      for (j in seq_len(nrow(r1[[i]]))) {
        if (r1[[i]]$upper.es[j] < lower.equi) {
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat2[j], r1[[i]]$treat1[j], sep = " > ")
          #
          ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the second position and the treatment 2 in the first position (i.e. t2>t1)
          r1[[i]]$rank1[j] <- 2
          r1[[i]]$rank2[j] <- 1
        }
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the first position and the treatment 2 in the second position (i.e. t1>t2)
        else if (r1[[i]]$lower.es[j] > upper.equi) {
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat1[j], r1[[i]]$treat2[j], sep = " > ")
          #
          r1[[i]]$rank1[j] <- 1 
          r1[[i]]$rank2[j] <- 2
        }
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the second position and the treatment 2 in the first position (i.e. t2>t1)
        ## here 'no_effect2' can be either the null value (if relax=FALSE (default)) or it can be the upper bound of ROE if relax=T.
        ## the relaxed criterion allows for more "wins" to be identified from the TTC as in this case the statistical significance of the effect does not alter the preference
        
        else if (r1[[i]]$lower.es[j] < lower.equi & (r1[[i]]$es[j] < lower.equi) & (r1[[i]]$upper.es[j] < no_effect2)) { ## can be combined with first case
          
          r1[[i]]$rank_text[j] <- paste(r1[[i]]$treat2[j], r1[[i]]$treat1[j], sep = " > ")
          
          r1[[i]]$rank1[j] <- 2 
          
          r1[[i]]$rank2[j] <- 1
          
        }
        #
        # For the pairwise comparison between treatments t1 and t2 the
        # treatment 1 is in the first position and the treatment 2 in the
        # second position (i.e. t1 > t2).
        # Here 'no_effect1' can be either the null value, if relax = FALSE
        # (default), or it can be the lower bound of ROE if relax = TRUE.
        # The relaxed criterion allows for more "wins" to be identified from
        # the TTC as in this case the statistical significance of the effect
        # does not alter the preference.
        #
        else if (r1[[i]]$upper.es[j] > upper.equi &
                 r1[[i]]$es[j] > upper.equi &
                 r1[[i]]$lower.es[j] > no_effect1) { # can be combined with second case
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat1[j], r1[[i]]$treat2[j], sep = " > ")
          #
          r1[[i]]$rank1[j] <- 1 
          r1[[i]]$rank2[j] <- 2
        }
        else {
          # Act upon ties
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat1[j], r1[[i]]$treat2[j], sep = " = ")
          #
          r1[[i]]$rank1[j] <- 1
          r1[[i]]$rank2[j] <- 1
        }
      }
    }
    else {
      # Since 'small.values' are desirable the smaller the treatment effect the
      # more likely that for the comparison t1 vs t2, t1 is preferred.
      for (j in seq_len(nrow(r1[[i]]))) {
        ## for the pairwise comparison between treatments t1 and t2 the treatment 1 is in the first position and the treatment 2 in the second position (i.e. t1>t2)
        if (r1[[i]]$upper.es[j] < lower.equi) {
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat1[j], r1[[i]]$treat2[j], sep = " > ")
          #
          r1[[i]]$rank1[j] <- 1 
          r1[[i]]$rank2[j] <- 2
        }
        else if (r1[[i]]$lower.es[j] > upper.equi) {
          # For the pairwise comparison between treatments t1 and t2
          # the treatment 1 is in the second position and the treatment 2
          # in the first position (i.e. t2 > t1)
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat2[j], r1[[i]]$treat1[j], sep = " > ")
          #
          r1[[i]]$rank1[j] <- 2 
          r1[[i]]$rank2[j] <- 1
        }
        #
        # For the pairwise comparison between treatments t1 and t2 the
        # treatment 1 is in the first position and the treatment 2 in the
        # second position (i.e. t1 > t2).
        # Here 'no_effect2' can be either the null value, if relax = FALSE
        # (default), or it can be the upper bound of ROE if relax = TRUE.
        # The relaxed criterion allows for more "wins" to be identified from
        # the TTC as in this case the statistical significance of the effect
        # does not alter the preference.
        #
        else if (r1[[i]]$lower.es[j] < lower.equi &
                 r1[[i]]$es[j] < lower.equi &
                 r1[[i]]$upper.es[j] < no_effect2) { # can be combined with first case
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat1[j], r1[[i]]$treat2[j], sep = " > ")
          #
          r1[[i]]$rank1[j] <- 1
          r1[[i]]$rank2[j] <- 2
        }
        #
        # For the pairwise comparison between treatments t1 and t2 the
        # treatment 1 is in the second position and the treatment 2 in the
        # first position (i.e. t2 > t1).
        # Here 'no_effect1' can be either the null value, if relax = FALSE
        # (default), or it can be the upper bound of ROE if relax = TRUE.
        # The relaxed criterion allows for more "wins" to be identified from
        # the TTC as in this case the statistical significance of the effect
        # does not alter the preference.
        #
        else if (r1[[i]]$upper.es[j] > upper.equi &
                 r1[[i]]$es[j] > upper.equi &
                 r1[[i]]$lower.es[j] > no_effect1) { # can be combined with second case
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat2[j], r1[[i]]$treat1[j], sep = " > ")
          #
          r1[[i]]$rank1[j] <- 2
          r1[[i]]$rank2[j] <- 1
        }
        else {
          # In any other case the tcc indicates a tie 
          r1[[i]]$rank_text[j] <-
            paste(r1[[i]]$treat1[j], r1[[i]]$treat2[j], sep = " = ")
          #
          r1[[i]]$rank1[j] <- 1 
          r1[[i]]$rank2[j] <- 1
        }
      }
    }
    
    ## END OF tcc ## 
    
    #
    #
    # Gather the results across all pairwise comparisons
    #
    #
    
    x[[i]] <- cbind.data.frame(
      rep(r1[[i]]$studlab, 2),
      rep(r1[[i]]$studlab_orig, 2),
      rbind.data.frame(r1[[i]]$treat1, r1[[i]]$treat2),
      rbind.data.frame(r1[[i]]$rank1, r1[[i]]$rank2)
    )
    #
    names(x[[i]]) <- c("studlab", "studlab_orig", "treat", "rank")
    #
    x[[i]] <- x[[i]] %>% arrange(x[[i]]$rank)
    #
    x[[i]]$outcome <- ifelse(sum(x[[i]]$rank) == 2, "tie", "winner")
    #
    ind[[i]] <- paste(unique(x[[i]]$outcome), i, sep = ".")
  }
  
  #
  #
  # Create a data frame in a wide format with the information for all
  # comparisons   
  #
  #
  
  dat <- bind_rows(r1, .id = "column_label")
  
  dat$column_label <- NULL
  
  #
  # Check if only ties were identified from tcc and return a warning if so.
  # In this case the model fit from mtrank() will fail to converge.
  all.ties <- unique(dat$rank1 + dat$rank2) == 2
  
  if (isTRUE(all.ties))
    warning("Only ties were identified through the treatment choice criterion. ",
            "This can yield into convergence problems when using mtrank().")

  # Organize the list x so that all studies with "ties" come first and studies
  # with "wins" second. This is because the rankings() function that is used to
  # perform the final transformation requires (or its a bug?) this.
  #
  names(x) <- as.character(unlist(ind))
  #
  x <- x[order(names(x))]
  #
  names(x) <- NULL
  
  #
  # Save the order of the studies and use it for the "as.rankings" function to
  # group the preferences
  #
  for (i in seq_len(length(x)))
    index[[i]] <- unique(x[[i]]$studlab)
  #
  index <- unlist(index)
  
  #
  # Create a data frame with all the elements of list x
  #
  data_final <- bind_rows(x, .id = "column_label")
  data_final$column_label <- as.numeric(data_final$column_label)
  
  print(names(data_final))
  print(data_final %>% filter(studlab == "PEACE"))
  
  #
  # Ungrouped treatment preferences:
  # the number of rows is equal to the number of study-specific pairwise
  # comparisons in the network
  #
  ungrouped.preferences <-
    rankings(data_final, id = "column_label", item = "treat", rank = "rank")
  #
  # Ensure that preferences from multi-arm studies are grouped together using
  # the grouping criterion based on "index".
  # The number of rows is equal to the number of studies in the network.
  #
  # The output of this function gives the grouped study preferences.
  # The order that the comparisons appear correspond to the
  # order of rows in the 'index_match' which is created below.
  #  
  grouped.preferences <- as.rankings(ungrouped.preferences, index = index)
  #
  print(coef(PlackettLuce(ungrouped.preferences)) ==
          coef(PlackettLuce(grouped.preferences)))
  print(coef(PlackettLuce(ungrouped.preferences)))
  #
  print(coef(PlackettLuce(ungrouped.preferences2)) == 
          coef(PlackettLuce(grouped.preferences2)))
  print(coef(PlackettLuce(ungrouped.preferences2)))
  #
  # Save the results
  # (i) data_pref: the data transformed in a preference format
  # (ii) index.match: a data frame which contains the matching in the names of
  #      "data_pref" and the actual study names, this is helpful to keep track
  #      on which study of the original data generates the ith preference of
  #      the "data_pref" output.              
  #
  
  #
  # Match the study index with the original study name 
  #
  index_match <-
    data.frame(index = data_final$studlab, study_name = data_final$studlab_orig)
  #
  index_match <-
    index_match[!duplicated(index_match$index, index_match$study_name), ]
  index_match  <- index_match[order(as.numeric(index_match$index)),]
  #
  row.names(index_match) <- NULL
  
  #
  # Create output list
  #
  res <- list(grouped.preferences = grouped.preferences,
              index.match = index_match,
              data = dat,
              #
              lower.equi = lower.equi,
              upper.equi = upper.equi,
              #
              sm = sm,
              level = level,
              #
              ungrouped.preferences = ungrouped.preferences,
              all.ties = all.ties,
              small.values = small.values,
              #
              trts = sort(unique(c(dat$treat1, dat$treat2))),
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
