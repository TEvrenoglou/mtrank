#' Line graph showing the results of \code{\link{mtrank}} across different
#' smallest worthwhile difference (SWD) values
#' 
#' @description
#' This function produces a line graph that visualizes the results of
#' \code{\link{mtrank}} in terms of either abilities or probabilities across
#' different smallest worthwhile difference (SWD) values.
#' 
#' @param x An object of class \code{\link{mtrank}}.
#' @param swd A numeric vector of SWD values to be used for the sensitivity
#'   analysis.
#' @param swd.ref A numeric SWD value to be used as the reference for sorting
#'   treatments in the final graph. This value must be included in
#'   \code{swd}.
#' @param small.values A character string specifying whether small treatment
#'   effects indicate a beneficial (\code{"desirable"}) or harmful
#'   (\code{"undesirable"}) effect; can be abbreviated.
#' @param type The metric to be used for plotting the results of the
#'   sensitivity analysis. Two options are available: the default is
#'   \code{"probability"}, which plots results in terms of normalized abilities; 
#'   the alternative is \code{"ability"}, which plots results in terms of
#'   ability estimates. Both options can be abbreviated. 
#' @param k A numeric value indicating the number of treatments to be plotted.
#'   By default, all available treatments are shown. For large networks, it is
#'   advisable to limit the number of treatments to improve readability.
#'   If specified, the first \code{k} treatments based on the hierarchy at
#'   \code{swd.ref} will be plotted. 
#' @param backtransf A logical value indicating whether to display log-ability
#'   estimates (\code{FALSE}, default) or back-transformed ability estimates
#'   on the natural scale (\code{TRUE}). This argument is ignored if
#'   \code{type = "probability"}.
#' @param linewidth A numeric value specifying the width of the lines
#'   (default: 1.1).
#' @param point.size A numeric value specifying the size of the points
#'   (default: 2).
#' @param \dots Additional arguments passed to \code{\link[mtrank]{mtrank}}.
#' 
#' @details
#' This function creates a line graph to visualize probability or ability
#' estimates obtained from \code{\link{mtrank}} across different SWD values.
#' The order of treatments in the graph is based on their hierarchy at the
#' reference SWD value (\code{swd.ref}).
#' 
#' @return
#' A \code{ggplot} object.
#' 
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Ruecker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria,
#' \url{https://arxiv.org/abs/2406.10612}
#'
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
#' #
#' ranks <- tcc(net, swd = 1.20, small.values = "undesirable")
#' #
#' fit <- mtrank(ranks)
#' #
#' # Perform a sensitivity analysis across different SWD values assuming that
#' # 1.20 is the reference value
#' swd.vec <- seq(1.10, 1.50, by = 0.10)
#' swd.ref <- 1.20
#' # plot all the treatments in the network
#' linegraph(fit, swd = swd.vec, swd.ref = swd.ref)
#' # plot only the first three treatments in the order appearing at the
#' # 'swd.ref' value
#' linegraph(fit, swd = swd.vec, swd.ref = swd.ref, k = 3)
#' # plot in terms of ability estimates 
#' linegraph(fit, swd = swd.vec, swd.ref = swd.ref, type = "ability")
#'    
#' @export

linegraph <- function(x, 
                      swd,
                      swd.ref,
                      small.values = x$small.values,
                      type = "probability",
                      k = length(x$trts),
                      backtransf = FALSE,
                      linewidth = 1.1,
                      point.size = 2,
                      ...) {
  #
  chkclass(x, "mtrank")
  chknumeric(swd)
  chknumeric(swd.ref)
  chknumeric(k, min = 0)
  #
  type <- setchar(type,val=c("ability","probability"))
  #
  E <- which(is_zero(swd - swd.ref))
  #
  if (length(E) == 0)
    stop("The reference SWD value must be part of the SWD values in 'swd'.",
         call. = FALSE)
  
  # Get rid of warning "no visible global function definition"
  #
  desc <- log_ability <- probability <- swd.breaks <- treatment <- NULL
  #
  ests <- prob <- vector("list", length(swd))
  #
  net.obj <- attributes(x)$net.obj
  #
  for (i in seq_along(swd)) {
    swd.i <- swd[i]
    tcc.i <- tcc(net.obj, small.values = small.values, swd = swd[i])
    #
    if (isFALSE(tcc.i$all.ties)) {
      mtr.i <- mtrank(tcc.i, ...)
      #
      prob[[i]] <- mtr.i$probabilities
      prob[[i]]$swd <- swd.i
      #
      ests[[i]] <- mtr.i$estimates %>% 
        arrange(desc(log_ability)) %>% 
        mutate(swd = swd.i)
    }
    else {
      prob[[i]] <- NA
      ests[[i]] <- NA
    }
  }
  #
  prob <-
    Filter(function(x) !is.null(x) && !(is.atomic(x) && all(is.na(x))), prob)
  ests <-
    Filter(function(x) !is.null(x) && !(is.atomic(x) && all(is.na(x))), ests)
  #
  prob_new <- bind_rows(prob)
  ests_new <- bind_rows(ests)
  #
  treats <- prob[[E]]$treatment[seq_len(k)]
  #
  swd.breaks <- swd
  #
  if (type == "probability") {
    data <- prob_new %>% 
      filter(treatment %in% treats) %>% 
      mutate(treatment = factor(treatment, levels = treats))
    #
    graph <- ggplot(data, aes(x = swd, y = probability, color = treatment)) +
      geom_line(linewidth = linewidth) +
      geom_point(size = point.size) +
      theme_minimal() +
      xlab("SWD") +
      ylab("Probability") +
      ylim(c(0, 1)) +
      scale_x_continuous(breaks = swd.breaks) +
      guides(color = guide_legend(title = "Treatment"))
  }
  else if (type == "ability") {
    data <- ests_new %>% 
      filter(treatment %in% treats) %>% 
      mutate(treatment = factor(treatment, levels = treats))
    #
    if (isFALSE(backtransf)) {
      graph <- ggplot(data, aes(x = swd, y = log_ability, color = treatment)) +
        geom_line(linewidth = linewidth) +
        geom_point(size = point.size) +
        theme_minimal() +
        xlab("SWD") +
        ylab("log-abilities") +
        scale_x_continuous(breaks = swd.breaks) +
        guides(color = guide_legend(title = "Treatment"))
    }
    else {
      graph <- ggplot(data,
                      aes(x = swd, y = exp(log_ability), color = treatment)) +
        geom_line(linewidth = linewidth) +
        geom_point(size = point.size) +
        theme_minimal() +
        xlab("SWD") +
        ylab("Abilities") +
        scale_x_continuous(breaks = swd.breaks) +
        guides(color=guide_legend(title = "Treatment"))
    }
  }
  #
  E_swd <- which(!swd %in% unique(data$swd))
  #
  swd_all_ties <- unique(swd)[E_swd] 
  
  if (length(E_swd) != 0) {
    txt_swd <- paste(swd_all_ties, collapse = ", ")  
    #
    warning(paste("Values ", txt_swd," are not shown as for these the ",
                  "treatment choice criterion identified only ties."))
  }
  
  attr(graph, "data") <- data
  #
  graph
}
