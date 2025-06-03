#' mtrank: Brief overview
#'
#' @description
#' R package \bold{mtrank} enables the estimation of treatment hierarchies in 
#' network meta-analysis using a novel frequentist approach based on treatment 
#' choice criteria (TCC) and probabilistic ranking models, as described by
#' Evrenoglou et al. (2024). The TCC are defined using a rule based on the
#' smallest worthwhile difference (SWD). Using the defined TCC, the NMA
#' estimates (i.e., treatment effects and standard errors) are first transformed
#' into treatment preferences, indicating either a treatment preference (e.g.,
#' treatment A > treatment B) or a tie (treatment A = treatment B). These
#' treatment preferences are then synthesized using a probabilistic ranking
#' model, which estimates the latent ability parameter of each treatment and
#' produces the final treatment hierarchy. This parameter represents each
#' treatments ability to outperform all the other competing treatments in the
#' network. Here the terms "ability to outperform" indicates the propensity of
#' each treatment to yield clinically important and beneficial effects when
#' compared to all the other treatments in the network. Consequently, larger
#' ability estimates indicate higher positions in the ranking list.
#'
#' @details
#' The R package \bold{mtrank} provides the following functions:
#' \itemize{
#' \item Function \code{\link{tcc}} defines the TCC and produces a treatment
#'   preference format based on network meta-analysis estimates. 
#' \item Function \code{\link{mtrank}} synthesizes the output of the
#'   \code{\link{tcc}} function and estimates the final treatment ability.
#' \item Forest plots are created either for the results of the
#'   TCC (\code{\link{forest.tcc}}) or the final ability estimates
#'   (\code{\link{forest.mtrank}}).
#' \item Function \code{\link{fitted.mtrank}} uses the ability estimates
#'   obtained from \code{\link{mtrank}} to calculate pairwise probabilities
#'   that any treatment 'A' can be better, equal, or worse than any other
#'   treatment 'B' in the network.
#' \item The function \code{\link{linegraph}} visualizes the output of 
#'   \code{\link{mtrank}} across different SWD values. It serves as a
#'   sensitivity analysis to the initial choice of SWD.
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
#' models and treatment-choice criteria,
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
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_minimal
#'   xlab ylab ylim scale_x_continuous guides guide_legend   

"_PACKAGE"

NULL
