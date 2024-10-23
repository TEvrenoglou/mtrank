#' Network meta-analysis studying the incidence of diabetes
#' 
#' @description
#' Network meta-analysis comparing six antihypertensive drugs against the
#' incidence of diabetes.
#' 
#' @name diabetes
#' 
#' @docType data
#' 
#' @format A data frame with the following columns:
#' \tabular{rl}{
#' \bold{\emph{study}}\tab study label \cr
#' \bold{\emph{id}}\tab study id \cr
#' \bold{\emph{t}}\tab treatment label \cr
#' \bold{\emph{r}}\tab number of events \cr
#' \bold{\emph{n}}\tab group sample size \cr
#' \bold{\emph{rob}}\tab risk of bias assessment
#' }
#' 
#' @seealso \code{\link{mtrank}}, \code{\link{tcc}}
#' 
#' @source
#' Elliott W, Meyer P (2007):
#' Incident diabetes in clinical trials of antihypertensive drugs:
#' a network meta-analysis
#' \emph{Lancet},
#' \bold{369} 
#' 
#' @keywords datasets
#' 
#' @examples
#' data(diabetes)
#' #
#' ranks <- tcc(treat = t, studlab = study, event = r, n = n, data = diabetes,
#'   mcid = 1.20, sm = "OR", small.values = "desirable")
#' #
#' forest(ranks, treat = "ARB")

NULL
