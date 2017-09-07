# mlrMBO

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mlrMBO)](https://cran.r-project.org/package=mlrMBO)
[![Build Status](https://travis-ci.org/mlr-org/mlrMBO.png?branch=master)](https://travis-ci.org/mlr-org/mlrMBO)
[![Build status](https://ci.appveyor.com/api/projects/status/gvr607kqcl78qjq9/branch/master?svg=true)](https://ci.appveyor.com/project/jakob-r/mlrmbo/branch/master)
[![Coverage Status](https://img.shields.io/codecov/c/github/mlr-org/mlrMBO/master.svg)](https://codecov.io/github/mlr-org/mlrMBO?branch=master)
[![Monthly RStudio CRAN Downloads](https://cranlogs.r-pkg.org/badges/mlrMBO)](https://CRAN.R-project.org/package=mlrMBO)

Model-based optimization with [mlr](https://github.com/mlr-org/mlr/).

* [Documentation](https://mlr-org.github.io/mlrMBO/)
* [Issues, Requests and Bug Tracker](https://github.com/mlr-org/mlrMBO/issues)

# Installation

We reccomend to install the official release version:

```r
install.packages("mlrMBO")
```

For experimental use you can install the latest development version:

```r
devtools::install_github("mlr-org/mlrMBO")

```



# Introduction

![MBO demo](https://github.com/mlr-org/mlrMBO/blob/master/docs/articles/helpers/animation-.gif?raw=true)

`mlrMBO` is a highly configurable R toolbox for model-based / Bayesian optimization of black-box functions.

Features:

* EGO-type algorithms (Kriging with expected improvement) on purely numerical search spaces, see [Jones et al. (1998)](http://link.springer.com/article/10.1023/A:1008306431147)
* Mixed search spaces with numerical, integer, categorical and subordinate parameters
* Arbitrary parameter transformation allowing to optimize on, e.g., logscale
* Optimization of noisy objective functions
* Multi-Criteria optimization with approximated Pareto fronts
* Parallelization through multi-point batch proposals
* Parallelization on many parallel back-ends and clusters through [batchtools](https://github.com/mllg/batchtools) and [parallelMap](https://github.com/berndbischl/parallelMap)

For the *surrogate*, `mlrMBO` allows any regression learner from [`mlr`](https://github.com/mlr-org/mlr), including:
* Kriging aka. Gaussian processes (i.e. `DiceKriging`)
* random Forests (i.e. `randomForest`)
* and many more...

Various *infill criteria* (aka. _acquisition functions_) are available:
* Expected improvement (EI)
* Upper/Lower confidence bound (LCB, aka. statistical lower or upper bound)
* Augmented expected improvement (AEI)
* Expected quantile improvement (EQI)
* API for custom infill criteria

Objective functions are created with package [smoof](https://github.com/jakobbossek/smoof), which also offers many test functions for example runs or benchmarks.

Parameter spaces and initial designs are created with package [ParamHelpers](https://github.com/berndbischl/ParamHelpers).


# mlrMBO - How to Cite and Citing Publications

Please cite our [arxiv paper](https://arxiv.org/abs/1703.03373) (Preprint).
You can get citation info via `citation("mlrMBO")` or copy the following BibTex entry:

```bibtex
@article{mlrMBO,
  title = {{{mlrMBO}}: {{A Modular Framework}} for {{Model}}-{{Based Optimization}} of {{Expensive Black}}-{{Box Functions}}},
  url = {http://arxiv.org/abs/1703.03373},
  shorttitle = {{{mlrMBO}}},
  archivePrefix = {arXiv},
  eprinttype = {arxiv},
  eprint = {1703.03373},
  primaryClass = {stat},
  author = {Bischl, Bernd and Richter, Jakob and Bossek, Jakob and Horn, Daniel and Thomas, Janek and Lang, Michel},
  date = {2017-03-09},
}
```

Some parts of the package were created as part of other publications.
If you use these parts, please cite the relevant work appropriately:

* Multi-point proposals, including the new multi-objective infill criteria: [MOI-MBO: Multiobjective Infill for Parallel Model-Based Optimization](https://dx.doi.org/10.1007/978-3-319-09584-4_17)
* Multi-objective optimization: [Model-Based Multi-objective Optimization: Taxonomy, Multi-Point Proposal, Toolbox and Benchmark](https://dx.doi.org/10.1007/978-3-319-15934-8_5)
* Multi-objective optimization with categorical variables using the random forest as a surrogate: [Multi-objective parameter configuration of machine learning algorithms using model-based optimization](https://dx.doi.org/10.1109/SSCI.2016.7850221)
