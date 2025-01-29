#' Auxiliary function to transform data from paired-preference to
#' long-arm format
#' 
#' @description
#' Auxiliary function to transform data from paired-preference to
#' long-arm format
#' 
#' @param x An object of class "ppdata" (part of \code{\link{tcc}} object).
#'
#' @return
#' Data set in long-arm format that can be used as input to
#' \code{\link[PlackettLuce]{rankings}}.
#' 
#' @author Guido Schwarzer \email{guido.schwarzer@@uniklinik-freiburg.de}
#' 
#' @seealso \code{\link{tcc}}, \code{\link[PlackettLuce]{rankings}}
#' 
#' @examples
#' data(diabetes)
#' #
#' ranks <- tcc(treat = t, studlab = study, event = r, n = n, data = diabetes,
#'   mcid = 1.20, sm = "OR", small.values = "desirable")
#' #
#' pdat <- ranks$ppdata
#' #
#' ldat <- pp2long(pdat)
#' head(ldat)
#' 
#' library("PlackettLuce")
#' ungrouped.preferences <-
#'   rankings(ldat, id = "id", item = "treat", rank = "rank")
#' grouped.preferences <-
#'   as.rankings(ungrouped.preferences,
#'               index = as.numeric(as.factor(pdat$studlab)))
#' #
#' fit <- PlackettLuce(grouped.preferences)
#' #
#' coef(summary(fit, ref = ranks$reference.group))[, 1]
#' # Results stored in mtrank()
#' mtrank(ranks)$estimates$log_ability
#' 
#' @export pp2long

pp2long <- function(x) {
  
  chkclass(x, "ppdata")
  
  treat1 <- rank1 <- treat2 <- rank2 <-
    id <- grp <- studlab <- treat <- rank <- outcome <- NULL
  
  pdat1 <- x %>% mutate(treat = treat1, rank = rank1, grp = 1) %>%
    select(id, grp, studlab, treat, rank, outcome)
  #
  pdat2 <- x %>% mutate(treat = treat2, rank = rank2, grp = 2) %>%
    select(id, grp, studlab, treat, rank, outcome)
  #
  ldat <- rbind(pdat1, pdat2) %>% arrange(id, grp)
  class(ldat) <- "data.frame"
    
  ldat
}
