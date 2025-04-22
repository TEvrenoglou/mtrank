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
#' pw <- pairwise(studlab = study, treat = t,
#'   n = n, event = r, data = diabetes, sm = "OR")
#' #
#' net <- netmeta(pw, reference.group = "PLA")
#' #
#' ranks <- tcc(net, mcid = 1.20, small.values = "desirable")
#' #
#' pdat <- ranks$ppdata
#' #
#' ldat <- pp2long(pdat)
#' head(ldat)
#' 
#' library("PlackettLuce")
#' preferences <- rankings(ldat, id = "id", item = "treat", rank = "rank")
#' #
#' fit <- PlackettLuce(preferences)
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
