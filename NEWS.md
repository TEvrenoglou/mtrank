## mtrank, version 0.2-0 (2025-04-dd)

### Major changes

* New treatment choice criterion (TCC) derived from network estimates replaces
  TCC derived from individual study results

* Main argument in R function tcc() must be a network meta-analysis object
  created with netmeta() from R package **netmeta**

### User-visible changes

* R function paired_pref() renamed to fitted.mtrank()

* tcc():
  - single new argument 'x' replaces arguments 'treat', 'event', 'n',
    'mean', 'sd', 'data', and 'studlab'

* forest.tcc():
  - argument 'treat' replaced by 'reference.group'
  - new argument 'baseline.reference'
  - new arguments 'col.winner' and 'col.tie'
  - argument 'fill.lower.equi' replaced by 'fill.mcid.below.null'
  - argument 'fill.upper.equi' replaced by 'fill.mcid.above.null'


## mtrank, version 0.1-1 (2025-02-26)

### Bug fixes

* tcc():
  - use correct 'no_effect1' and 'no_effect2' values for relative effect
    measures if argument 'relax = TRUE'


## mtrank, version 0.1-0 (2025-01-31)

First version released on CRAN
