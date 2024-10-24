% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest.tcc.R
\name{forest.tcc}
\alias{forest.tcc}
\title{Forest plot of study-specific preferences or ties for treatment choice
criterion}
\usage{
\method{forest}{tcc}(
  x,
  treat = NULL,
  backtransf = FALSE,
  leftcols = "studlab",
  leftlabs = NULL,
  rightcols = c("effect", "ci"),
  lty.equi = gs("lty.equi"),
  col.equi = gs("col.equi"),
  fill.equi = gs("fill.equi"),
  fill.lower.equi = fill.equi,
  fill.upper.equi = rev(fill.equi),
  header.line = TRUE,
  col.subgroup = "black",
  ...
)
}
\arguments{
\item{x}{An object of class \code{\link{tcc}}.}

\item{treat}{A treatment of interest. If specified it returns a forest plot
for all study specific effects related to \code{treat}. If NULL (default)
it generates a forest plot for all study-specific effects in the network.}

\item{backtransf}{A logical indicating whether results should be
back transformed. If \code{backtransf =
TRUE} (default), results for \code{sm = "OR"} are printed as odds
ratios rather than log odds ratios, for example.}

\item{leftcols}{A character vector specifying columns
to be printed on the left side of the forest plot
(see \code{\link[meta]{forest.meta}}).}

\item{leftlabs}{A character vector specifying labels for
columns on left side of the forest plot.}

\item{rightcols}{A character vector specifying columns
to be printed on the right side of the forest plot
(see \code{\link[meta]{forest.meta}}).}

\item{lty.equi}{Line type (limits of equivalence).}

\item{col.equi}{Line colour (limits of equivalence).}

\item{fill.equi}{Colour(s) for area between limits of equivalence
or more general limits.}

\item{fill.lower.equi}{Colour of area between lower limit(s) and
reference value. Can be equal to the number of lower limits or
the number of limits plus 1 (in this case the the region between
minimum and smallest limit is also filled).}

\item{fill.upper.equi}{Colour of area between reference value and
upper limit(s). Can be equal to the number of upper limits or the
number of limits plus 1 (in this case the region between largest
limit and maximum is also filled).}

\item{header.line}{A logical value indicating whether to print a
header line or a character string ("both", "below", "").}

\item{col.subgroup}{The colour to print information on subgroups, i.e.,
pairwise comparisons.}

\item{\dots}{Additional arguments (passed on to
\code{\link[meta]{forest.meta}}).}
}
\value{
A forest plot that shows the study-specific treatment effects and the range
of equivalence defined from the function \code{\link{tcc}}. Coloured
with red are the treatment effects which according to the \code{\link{tcc}}
function were producing a treatment preference and coloured with
black those treatment effects producing a treatment tie.
}
\description{
This function produces a forest plot for all (or some specific) study
specific comparisons and visualizes the treatment preference or ties
which are defined from the treatment choice criterion in \code{\link{tcc}}.
}
\details{
This function produces forest plots for the study specific treatment effects
in the network. The legend of these graphs specifies which treatment effects
were identified as treatment preferences and which as treatment ties.
Additionally, the respective range of equivalence defined at the function
\code{\link{tcc}} is also visualized for each forest plot. The argument
\code{treat} is optional. If specified it returns only the forest plots in
terms of the study-specific treatment effects related to the specified
\code{treat}. If NULL it will return the forest plots in terms of all
study-specific treatment effects in the network. We recommend that the
specification of the argument \code{treat} especially for busy networks
which contain many direct comparisons.
}
\examples{
data(diabetes)
#
ranks <- tcc(treat = t, studlab = study, event = r, n = n, data = diabetes,
  mcid = 1.20, sm = "OR", small.values = "desirable")
#
forest(ranks)
forest(ranks, treat = "ARB")

}
\references{
Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024):
Producing treatment hierarchies in network meta-analysis using probabilistic
models and treatment-choice criteria.
\url{https://arxiv.org/abs/2406.10612}
}
