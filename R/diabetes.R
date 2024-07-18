#' Diabetes dataset
#' 
#' @description
#' Diabetes dataset
#' 
#' @name diabetes
#' 
#' @docType data
#' 
#' @format A data frame with the following columns:
#' \tabular{rl}{
#' \bold{\emph{study}}\tab study label \cr
#' \bold{\emph{id}}\tab id \cr
#' \bold{\emph{t}}\tab treatment label \cr
#' \bold{\emph{y}}\tab mean \cr
#' \bold{\emph{r}}\tab number of events \cr
#' \bold{\emph{n}}\tab group sample size \cr
#' \bold{\emph{rob}}\tab risk of bias assessment \cr
#' \bold{\emph{indirectness}}\tab indirectness \cr
#' \bold{\emph{year}}\tab year
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
#' data(diabetes)
#' #
#' ranks <- tcc(treat = t, studlab = study, event = r, n = n, data = diabetes,
#'   mcid = 1.20, sm = "OR", small.values = "desirable")
#' #
#' forest(ranks, treat = "arb")


NULL
