#' Network meta-analysis of adjuvant treatments to levodopa therapy
#' for Parkinson's disease
#' 
#' @description
#' This dataset contains data from a Cochrane review assessing
#' efficacy and safety of three drug classes as adjuvant treatment to
#' levodopa therapy in patients with Parkinson’s disease and motor
#' complications (Stowe et al., 2010). The authors conducted three
#' pairwise meta-analyses comparing dopamine agonists,
#' catechol-O-methyl transferase inhibitors (COMTIs), and monoamine
#' oxidase type B inhibitors (MAOBIs), respectively, with placebo.
#' 
#' The primary outcome was the mean reduction of the time spent in a
#' relatively immobile ’off’ phase (mean off-time), calculated in
#' hours per day. Relative treatment effects were expressed as mean
#' difference. Data on this outcome were available for 5,331 patients
#' from 28 studies comparing an active treatment with placebo and one
#' three-arm study comparing two active treatments with placebo.
#' 
#' @name Stowe2010_long
#' 
#' @docType data
#' 
#' @format A data frame with the following columns:
#' \tabular{rl}{
#' \bold{\emph{study}}\tab study label \cr
#' \bold{\emph{id}}\tab study id \cr
#' \bold{\emph{t}}\tab treatment \cr
#' \bold{\emph{y}}\tab treatment effect \cr
#' \bold{\emph{sd}}\tab standard deviation \cr
#' \bold{\emph{n}}\tab sample size \cr
#' \bold{\emph{rob}}\tab risk of bias assessment \cr
#' \bold{\emph{indirectness}}\tab indirectness
#' }
#' 
#' @seealso \code{\link[netmeta]{Stowe2010}}, \code{\link{mtrank}},
#'   \code{\link{tcc}}
#' 
#' @source
#' Stowe R, Ives N, Clarke CE, Deane K, Hilten V, Wheatley K, et
#' al. (2010):
#' Evaluation of the efficacy and safety of adjuvant treatment to
#' levodopa therapy in Parkinson's disease patients with motor
#' complications.
#' \emph{Cochrane Database of Systematic Reviews}
#' 
#' @keywords datasets
#' 
#' @examples
#' data(Stowe2010_long)
#' #
#' ranks <- tcc(treat = t, studlab = study, mean = y, n = n, sd = sd,
#'   data = Stowe2010_long, sm = "MD", small.values = "desirable",
#'   mcid = 0.5)
#' #
#' model <- mtrank(ranks)
#' #
#' paired_pref(model,
#'    treat1 = "MAOBI", treat2 = "Dopamine Agonist", type = "better")


NULL
