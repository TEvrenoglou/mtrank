#' mtrank: Brief overview
#'
#' @description
#' R package \bold{mtrank} enables the production of clinically relevant
#' treatment hierarchies in network meta-analysis using a novel frequentist
#' approach based on treatment choice criteria (TCC) and probabilistic
#' ranking models, as described by Evrenoglou et al. (2024). The TCC are defined
#' using a rule based on the minimal clinically important difference. Using the
#' defined TCC, the study-level data (i.e., treatment effects and standard
#' errors) are first transformed into a preference format, indicating either a
#' treatment preference (e.g., treatment A > treatment B) or a tie (treatment A
#' = treatment B). The preference data are then synthesized using a
#' probabilistic ranking model, which estimates the latent ability parameter
#' of each treatment and produces the final treatment hierarchy. This parameter
#' represents each treatment’s ability to outperform all the other competing
#' treatments in the network. Consequently, larger ability estimates indicate
#' higher positions in the ranking list.
#'
#' @details
#' The R package \bold{mtrank} provides the following functions:
#' \itemize{
#' \item Function \code{\link{tcc}} defines the TCC and transforms the
#'   study-specific relative treatment effects into a preference format.
#' \item Function \code{\link{mtrank}} synthesizes the output of the
#'   \code{\link{tcc}} function and estimates the final treatment ability.
#' \item Forest plots are created either for the results of the
#'   TCC (\code{\link{forest.tcc}}) or the final ability estimates
#'   (\code{\link{forest.mtrank}}).
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
