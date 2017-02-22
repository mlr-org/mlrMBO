# mlrMBO

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mlrMBO)](https://cran.r-project.org/package=mlrMBO)
[![Build Status](https://travis-ci.org/mlr-org/mlrMBO.png?branch=master)](https://travis-ci.org/mlr-org/mlrMBO)
[![Build status](https://ci.appveyor.com/api/projects/status/gvr607kqcl78qjq9/branch/master?svg=true)](https://ci.appveyor.com/project/jakob-r/mlrmbo/branch/master)
[![Coverage Status](https://img.shields.io/codecov/c/github/mlr-org/mlrMBO/master.svg)](https://codecov.io/github/mlr-org/mlrMBO?branch=master)

Model-based optimization with [mlr](https://github.com/mlr-org/mlr/).

* [Documentation](https://mlr-org.github.io/mlrMBO/)
* [Issues, Requests and Bug Tracker](https://github.com/mlr-org/mlrMBO/issues)

# Installation

`mlrMBO` currently needs the development versions of the packages [mlr](https://github.com/mlr-org/mlr/) and [ParamHelpers](https://github.com/berndbischl/ParamHelpers).
To install `mlrMBO` with all dependencies, run the following lines:

```r
devtools::install_github("berndbischl/ParamHelpers")
devtools::install_github("mlr-org/mlr")
devtools::install_github("mlr-org/mlrMBO")
```

# Introduction

![MBO demo](https://raw.githubusercontent.com/mlr-org/mlrMBO/master/docs/articles/helpers/animation_files/figure-html/animation-.gif)

`mlrMBO` is a toolbox for Model-Based Optimization for Black-Box functions in R.
It is suitable for the following expensive optimization tasks:
* Global optimization of functions on numeric search space.
* Optimization on mixed search space including categorical variables.
* Multi-Criteria Optimization through approximated Pareto fronts.
* Parallelization through multi-point proposals.
* Optimization of noisy objective functions.

With a highly configurable algorithm it allows for optimization of many practical optimization tasks that are of a black-box nature and are very time-consuming.
It implements the popular EGO algorithm by [Jones et al. (1998)](http://link.springer.com/article/10.1023/A:1008306431147) for optimizing black-box functions on numerical search spaces which uses *Gaussian processes* coupled with the *expected improvement* criterion for point proposal.
But thanks to it's modular design it can be configured to solve many more optimization tasks.

For the regression method of the *surrogate* `mlrMBO` relies on [`mlr`](https://github.com/mlr-org/mlr) so you are free to use:
* Kriging aka. Gaussian processes (i.e. `DiceKriging`)
* random Forests (i.e. `randomForest`)
* and many more...

For the point proposal various *infill criteria* (aka. _acquisition functions_) are available:
* Expected Improvement
* Upper/Lower Confidence Bound (aka. Statistical Lower or Upper Bound)
* Augmented Expected Improvement
* Expected Quantile Improvement
* Accessible interface to implement your own infill criterion.
