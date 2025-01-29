# mtrank: Treatment Hierarchies in Network Meta-Analysis using Probabilistic Models and Treatment-Choice Criteria

Official Git repository of R package **mtrank**

[![License: GPL (>=2)](https://img.shields.io/badge/license-GPL-blue)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![CRAN Version](https://www.r-pkg.org/badges/version/mtrank)](https://cran.r-project.org/package=mtrank)
[![GitHub develop](https://img.shields.io/badge/develop-0.1--1-purple)](https://img.shields.io/badge/develop-0.1--1-purple)
[![Monthly Downloads](https://cranlogs.r-pkg.org/badges/mtrank)](https://cranlogs.r-pkg.org/badges/mtrank)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/mtrank)](https://cranlogs.r-pkg.org/badges/grand-total/mtrank)


## Authors

[Theodoros Evrenoglou](https://orcid.org/0000-0003-3336-8058),
[Guido Schwarzer](https://orcid.org/0000-0001-6214-9087)


## Description

**mtrank** is an R package to produce clinically relevant treatment hierarchies in network meta-analysis using a novel frequentist approach based on treatment choice criteria (TCC) and probabilistic ranking models ([Evrenoglou et al., 2024](https://arxiv.org/abs/2406.10612)). The TCC are defined using a rule based on the minimal clinically important difference. Using the defined TCC, the study-level data (i.e., treatment effects and standard errors) are first transformed into a preference format, indicating either a treatment preference (e.g., *treatment A* > *treatment B*) or a tie (*treatment A* = *treatment B*). The preference data are then synthesized using a probabilistic ranking model, which estimates the latent ability parameter of each treatment and produces the final treatment hierarchy. This parameter represents each treatment’s ability to outperform all the other competing treatments in the network. Consequently, larger ability estimates indicate higher positions in the ranking list.


## Installation

<!--
### Current stable [![CRAN Version](https://www.r-pkg.org/badges/version/mtrank)](https://cran.r-project.org/package=mtrank) release:
```r
install.packages("mtrank")
```
-->

### Current [![GitHub develop](https://img.shields.io/badge/develop-0.1--1-purple)](https://img.shields.io/badge/develop-0.1--1-purple) release on GitHub:

Installation using R package
[**remotes**](https://cran.r-project.org/package=remotes):
```r
install.packages("remotes")
remotes::install_github("TEvrenoglou/mtrank", ref = "develop")
```

### Bug Reports:

```r
bug.report(package = "mtrank")
```

The bug.report function is not supported in RStudio. Please send an email to Theodoros Evrenoglou <<theodoros.evrenoglou@uniklinik-freiburg.de>> if you use RStudio.

You can also report bugs on GitHub under [Issues](https://github.com/TEvrenoglou/mtrank/issues/).


### Reference

[Evrenoglou T, Nikolakopoulou A, Schwarzer G, Rücker G, Chaimani A (2024): Producing treatment hierarchies in network meta-analysis using probabilistic models and treatment-choice criteria. Preprint on ArXiv.](https://doi.org/10.48550/arXiv.2406.10612)
