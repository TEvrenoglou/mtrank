## mtrank, version 0.2-0 (2025-04-dd)

### Major changes

* New treatment choice criterion (TCC) derived from network estimates replaces
  TCC derived from individual study results

* Main argument in R function tcc() must be a network meta-analysis object
  created with netmeta() from R package **netmeta**

### User-visible changes

* R function paired_pref() renamed to fitted.mtrank()

* R function linegraph() added

* Argument 'reference.group' was removed from function mtrank()

* tcc():
  - single new argument 'x' replaces arguments 'treat', 'event', 'n',
    'mean', 'sd', 'data', and 'studlab'
  - arguments 'mcid', 'mcid.below.nukk' ,'mcid.above.null' renamed to 
  'swd', 'swd.below.null', 'swd.above.null'

* forest.tcc():
  - argument 'treat' replaced by 'reference.group'
  - new argument 'baseline.reference'
  - new arguments 'col.winner' and 'col.tie'
  - argument 'fill.lower.equi' replaced by 'fill.swd.below.null'
  - argument 'fill.upper.equi' replaced by 'fill.swd.above.null'

### Bug fixes

* mtrank():
  - use 'method="BFGS"' internally to avoid errors due to convergence failure


## mtrank, version 0.1-1 (2025-02-26)

### Bug fixes

* tcc():
  - use correct 'no_effect1' and 'no_effect2' values for relative effect
    measures if argument 'relax = TRUE'


## mtrank, version 0.1-0 (2025-01-31)

First version released on CRAN
