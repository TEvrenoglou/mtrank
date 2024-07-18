#' Antidepressants dataset
#' 
#' @description
#' Antidepressants dataset
#' 
#' @name antidepressants
#' 
#' @docType data
#' 
#' @format A data frame with the following columns:
#' \tabular{rl}{
#' \bold{\emph{studyid}}\tab study id \cr
#' \bold{\emph{armid}}\tab armid \cr
#' ... \cr
#' \bold{\emph{t}}\tab treatment
#' }
#' 
#' @seealso \code{\link{mtrank}}, \code{\link{tcc}}
#' 
#' @source
#' Surname A, Surname Z. (year):
#' Title
#' \emph{Journal name},
#' \bold{vol}, p1--p2 
#' 
#' @keywords datasets
#' 
#' @examples
#' data(antidepressants)
#' #
#' ranks <- tcc(treat = drug_name, studlab = studyid,
#'   event = responders, n = ntotal, data = antidepressants,
#'   mcid = 1.25, sm = "OR", small.values = "undesirable")
#' #
#' model <- mtrank(ranks)
#' #
#' paired_pref(model, type = "better",
#'   treat1 = "bupropion", treat2 = "escitalopram")


NULL
