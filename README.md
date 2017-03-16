# mlrMBO

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mlrMBO)](https://cran.r-project.org/package=mlrMBO)
[![Build Status](https://travis-ci.org/mlr-org/mlrMBO.png?branch=master)](https://travis-ci.org/mlr-org/mlrMBO)
[![Build status](https://ci.appveyor.com/api/projects/status/gvr607kqcl78qjq9/branch/master?svg=true)](https://ci.appveyor.com/project/jakob-r/mlrmbo/branch/master)
[![Coverage Status](https://img.shields.io/codecov/c/github/mlr-org/mlrMBO/master.svg)](https://codecov.io/github/mlr-org/mlrMBO?branch=master)

Model-based optimization with [mlr](https://github.com/mlr-org/mlr/).

* [Documentation](https://mlr-org.github.io/mlrMBO/)
* [Issues, Requests and Bug Tracker](https://github.com/mlr-org/mlrMBO/issues)

# Installation

```r
install.packages("mlr-org/mlrMBO")
```

# Introduction

![MBO demo](https://raw.githubusercontent.com/mlr-org/mlrMBO/master/docs/articles/helpers/animation_files/figure-html/animation-.gif)

`mlrMBO` is a highly configurable R toolbox for model-based / Bayesian optimization of black-box functions.

Features:

* EGO-type algorithms (Kriging with expected improvement) on purely numerical search spaces, see [Jones et al. (1998)](http://link.springer.com/article/10.1023/A:1008306431147)
* Mixed search spaces with numerical, integer, categorical and subordinate parameters
* Arbitrary parameter transformation allowing to optimize on, e.g., logscale
* Optimization of noisy objective functions
* Multi-Criteria optimization with approximated Pareto fronts
* Parallelization through multi-point batch proposals
* Parallelization on many parallel back-ends and clusters through [batchtools](https://github.com/mllg/batchtools) and [parallelMap](https://github.com/berndbischl/parallelMap)

For the *surrogate*, mlrMBO`allows any regression learner from [`mlr`](https://github.com/mlr-org/mlr), including:
* Kriging aka. Gaussian processes (i.e. `DiceKriging`)
* random Forests (i.e. `randomForest`)
* and many more...

Various *infill criteria* (aka. _acquisition functions_) are available:
* Expected improvement (EI)
* Upper/Lower confidence bound (LCB, aka. statistical lower or upper bound)
* Augmented expected improvement (AEI)
* Expected quantile improvement (EQI)
* API for custom infill criteria
