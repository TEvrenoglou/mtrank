#' mtrank: Brief overview
#'
#' @description
#' The R package \bold{mtrank} provides a frequentist method that produces treatment hierarchies in network meta-analysis (NMA) using predefined treatment choice criteria (TCC) and probabilistic ranking models. The methodology is based on the manuscript by Evrenoglou et al. (2024).
#'
#' @details
#' The R package \bold{mtrank} provides the following functions:
#' \itemize{
#' \item Function \code{\link{tcc}} defines the TCC and transforms the
#'   study-specific relative treatment effects into a preference format.
#' \item Function \code{\link{mtrank}} synthesizes the output of the
#'   \code{\link{tcc}} function and estimates the final treatment ability.
#' \item Function \code{\link{forest}} visualizes either the results of the
#'   TCC (forest.tcc) or the final ability estimates (forest.mtrank).
#' \item Function \code{\link{paired_pref}} uses the ability estimates
#'   obtained from \code{\link{mtrank}} to calculate pairwise probabilities
#'   that any treatment 'A' can be better, equal, or worse than any other
#'   treatment 'B' in the network.
#' }
#' 
#' Type \code{help(package = "mtrank")} for a listing of R functions
#' available in \bold{mtrank}.
#'
#' Type \code{citation("mtrank")} on how to cite \bold{mtrank}
#' in publications.
#'
#' To report problems and bugs, please send an email to Theodoros
#' Evrenoglou <theodoros.evrenoglou@uniklinik-freiburg.de>.
#'
#' The development version of \bold{mtrank} is available on GitHub
#' \url{https://github.com/TEvrenoglou/mtrank}.
#'
#' @name mtrank-package
#'
#' @author Theodoros Evrenoglou <theodoros.evrenoglou@@uniklinik-freiburg.de>,
#'   Guido Schwarzer <guido.schwarzer@@uniklinik-freiburg.de>
#'
#' @references
#' Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
#' Producing treatment hierarchies in network meta-analysis using probabilistic
#' models and treatment-choice criteria.
#' \url{https://arxiv.org/abs/2406.10612}
#'
#' @keywords package
#'
#' @importFrom meta metagen ci gs forest pairwise
#' @importFrom netmeta netmeta netconnection invmat
#' @importFrom PlackettLuce PlackettLuce itempar rankings as.rankings
#' @importFrom dplyr %>% arrange bind_rows filter select mutate if_else
#'   relocate last_col
#' @importFrom magrittr %<>%
#' @importFrom utils packageVersion
#' @importFrom graphics legend segments text par
#' @importFrom stats complete.cases qnorm quantile

"_PACKAGE"

NULL
