#' mtrank: Brief overview
#'
#' @description
#' R package \bold{mtrank} provides ...
#'
#' @details
#' R package \bold{mtrank} provides ...
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
