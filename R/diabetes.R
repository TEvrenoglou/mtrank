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
#' Elliott WJ, Meyer PM (2007):
#' Incident diabetes in clinical trials of antihypertensive drugs:
#' A network meta-analysis
#' \emph{Lancet},
#' \bold{369}, 201--7,
#' \doi{10.1016/S0140-6736(07)60108-1}
#' 
#' @keywords datasets
#' 
#' @examples
#' data(diabetes)
#' head(diabetes)
#' #
#' \donttest{
#' pw <- pairwise(studlab = study, treat = t,
#'   n = n, event = r, data = diabetes, sm = "OR")
#' #
#' net <- netmeta(pw, reference.group = "PLA")
#' #
#' ranks <- tcc(net, swd = 1.20, small.values = "desirable")
#' #
#' forest(ranks)
#' forest(ranks, reference.group = "ARB", baseline.reference = FALSE)
#' }

NULL
