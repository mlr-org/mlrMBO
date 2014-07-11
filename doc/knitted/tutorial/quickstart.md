# Introductory example

**Info:** this guide gives you an overview of the typical structure of optimization with **mrMBO**. For a much more
detailed introduction see [the next chapter](introduction.md).

Here we provide a Quickstart example for you to make yourself familiar with **mlrMBO**. We aim to optimize the one dimensional Rastrigin function using model-based optimization. Instead of writing this function by hand, we make use of the soobench library, which offers a lot of common single objective optimization functions.


```splus
library(soobench)
library(mlr)
library(mlrMBO)
library(ParamHelpers)
obj.fun = rastrigin_function(1)
```

**Note:** Since all this stuff here is under heavy developement it might be neccessary to install the github developement version of the ParamHelpers package via the ```devtools::install_github``` function.

We decide ourself to use kriging as our surrgate model. Furthermore we use Expected Improvement (EI) as the infill criterion, i. e., the criterion which determines which point(s) of the objective function should be evaluated in further iterations (keep in mind, that using EI as the infill criterion needs the learner to support standard error estimation). Initially we let mbo generate an initial design of 30 points (the default algorithm is maximinLHS from package lhs).


```splus
par.set = extractParamSetFromSooFunction(obj.fun)

learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
control = makeMBOControl(iters = 10L, init.design.points = 30L)
control = setMBOControlInfill(control, crit = "ei")
```

Finally we start the optimization process and print the result object.


```splus
result = mbo(makeMBOFunction(obj.fun), par.set = par.set, learner = learner, control = control, show.info = TRUE)
```

```
## Computing y column(s) for design. Not provided.
## Loading required package: parallel
## [mbo] 0: x1=2.91 : y = 9.94 : 0.0 secs
## [mbo] 0: x1=-3.32 : y = 25 : 0.0 secs
## [mbo] 0: x1=0.494 : y = 20.2 : 0.0 secs
## [mbo] 0: x1=-3.69 : y = 27.1 : 0.0 secs
## [mbo] 0: x1=-4.65 : y = 37.4 : 0.0 secs
## [mbo] 0: x1=1.76 : y = 12.2 : 0.0 secs
## [mbo] 0: x1=4.5 : y = 40.3 : 0.0 secs
## [mbo] 0: x1=-2.8 : y = 14.9 : 0.0 secs
## [mbo] 0: x1=2.42 : y = 24.7 : 0.0 secs
## [mbo] 0: x1=-0.526 : y = 20.1 : 0.0 secs
## [mbo] 0: x1=0.789 : y = 8.21 : 0.0 secs
## [mbo] 0: x1=3.65 : y = 29.3 : 0.0 secs
## [mbo] 0: x1=-1.29 : y = 14.3 : 0.0 secs
## [mbo] 0: x1=-0.31 : y = 13.8 : 0.0 secs
## [mbo] 0: x1=4.32 : y = 33 : 0.0 secs
## [mbo] 0: x1=1.48 : y = 22.1 : 0.0 secs
## [mbo] 0: x1=-2.21 : y = 12.4 : 0.9 secs
## [mbo] 0: x1=-1.61 : y = 20.3 : 0.0 secs
## [mbo] 0: x1=-4.32 : y = 32.6 : 0.0 secs
## [mbo] 0: x1=-1.78 : y = 11.3 : 0.0 secs
## [mbo] 0: x1=3.86 : y = 18.6 : 0.0 secs
## [mbo] 0: x1=-3.34 : y = 26.4 : 0.0 secs
## [mbo] 0: x1=2.2 : y = 11.8 : 0.0 secs
## [mbo] 0: x1=0.0216 : y = 0.0928 : 0.0 secs
## [mbo] 0: x1=-0.946 : y = 1.47 : 0.0 secs
## [mbo] 0: x1=3.16 : y = 14.5 : 0.0 secs
## [mbo] 0: x1=-5 : y = 25 : 0.0 secs
## [mbo] 0: x1=1.15 : y = 5.29 : 0.0 secs
## [mbo] 0: x1=4.75 : y = 32.4 : 0.0 secs
## [mbo] 0: x1=-2.35 : y = 21.6 : 0.0 secs
## [mbo] 1: x1=-1.72 : y = 15 : 0.0 secs
## [mbo] 2: x1=-0.0502 : y = 0.497 : 0.0 secs
## [mbo] 3: x1=0.989 : y = 1 : 0.0 secs
## [mbo] 4: x1=4.07 : y = 17.5 : 0.0 secs
## [mbo] 5: x1=-1.04 : y = 1.36 : 0.0 secs
## [mbo] 6: x1=-0.00877 : y = 0.0153 : 0.0 secs
## [mbo] 7: x1=0.112 : y = 2.38 : 0.0 secs
## [mbo] 8: x1=-2 : y = 3.99 : 0.0 secs
## [mbo] 9: x1=1.99 : y = 3.98 : 0.0 secs
## [mbo] 10: x1=1.04 : y = 1.45 : 0.0 secs
```

```splus
print(result)
```

```
## Recommended parameters:
## x1=-0.00877
## Objective: y = 0.015
## 
## Optimization path
## 30 + 10 entries in total, displaying last 10 (or less):
##          x1        y dob eol error.message exec.time       ei error.model
## 31 -1.71758 14.97313   1  NA          <NA>         0 -0.10154        <NA>
## 32 -0.05024  0.49671   2  NA          <NA>         0 -0.76134        <NA>
## 33  0.98873  1.00265   3  NA          <NA>         0 -0.47515        <NA>
## 34  4.06821 17.45491   4  NA          <NA>         0 -0.18690        <NA>
## 35 -1.03802  1.36144   5  NA          <NA>         0 -0.39812        <NA>
## 36 -0.00877  0.01525   6  NA          <NA>         0 -0.31555        <NA>
## 37  0.11191  2.38435   7  NA          <NA>         0 -0.18567        <NA>
## 38 -1.99600  3.98717   8  NA          <NA>         0 -0.09983        <NA>
## 39  1.98862  3.98016   9  NA          <NA>         0 -0.08164        <NA>
## 40  1.04274  1.44581  10  NA          <NA>         0 -0.06965        <NA>
```
