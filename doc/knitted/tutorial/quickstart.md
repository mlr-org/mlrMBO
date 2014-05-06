# Introductory example

**Info:** this guide gives you an overview of the typical structure of optimization with **mrMBO**. For a much more
detailed introduction see [the next chapter](introduction.md).

Here we provide a Quickstart example for you to make yourself familiar with **mlrMBO**. We aim to optimize the one dimensional Rastrigin function using model-based optimization. Instead of writing this function by hand, we make use of the soobench library, which offers a lot of common single objective optimization functions.


```r
library(soobench)
library(mlr)
library(mlrMBO)
obj.fun = generate_rastrigin_function(1)
plot(obj.fun)
```

![plot of chunk unnamed-chunk-1](figs/quickstart/unnamed-chunk-1.png) 


We decide ourself to use kriging as our surrgate model. Furthermore we use Expected Improvement (EI) as the infill criterion, i. e., the criterion which determines which point(s) of the objective function should be evaluated in further iterations. Initially we generate a LHS design (latin hypercube) of 10 points.


```r
par.set = makeNumericParamSet(len = 1, id = "x", lower = lower_bounds(obj.fun), 
    upper = upper_bounds(obj.fun))

learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
control = makeMBOControl(propose.points = 1, iters = 5, infill.crit = "ei")

result = mbo(makeMBOFunction(obj.fun), par.set = par.set, learner = learner, 
    control = control, show.info = TRUE)
```

```
## Computing y column for design. Was not provided
## Loading required package: parallel
## [mbo] 0: x=4.07 : y=17.4
## [mbo] 0: x=2.69 : y=21.1
## [mbo] 0: x=-3.18 : y=16.1
## [mbo] 0: x=3.7 : y=27
## [mbo] 0: x=-4.16 : y=21.7
## [mbo] 0: x=2.34 : y=21.1
## [mbo] 0: x=-2.22 : y=13.2
## [mbo] 0: x=-0.285 : y=12.3
## [mbo] 0: x=0.726 : y=12
## [mbo] 0: x=-1.35 : y=17.5
## [mbo] 0: x=1.29 : y=14.1
## [mbo] 0: x=-3.54 : y=32.2
## [mbo] 0: x=-1.93 : y=4.71
## [mbo] 0: x=0.208 : y=7.41
## [mbo] 0: x=4.61 : y=39.1
## [mbo] 0: x=1.71 : y=15.2
## [mbo] 0: x=-2.93 : y=9.46
## [mbo] 0: x=-0.77 : y=9.34
## [mbo] 0: x=-4.53 : y=40.3
## [mbo] 0: x=3.38 : y=28.8
## [mbo] 1: x=1.02 : y=1.13
## [mbo] 2: x=0.956 : y=1.29
## [mbo] 3: x=1.16 : y=5.92
## [mbo] 4: x=-1.75 : y=12.9
## [mbo] 5: x=0.99 : y=0.999
```

```r
print(result)
```

```
## $x
## $x$x
## [1] 0.9903
## 
## 
## $y
## [1] 0.9993
## 
## $opt.path
## Optimization path
##   Dimensions: x=1/1, y=2
##   Length: 25
##   Add x values transformed: FALSE
## 
## $times
##  [1] 0.000 0.000 0.000 0.000 0.001 0.000 0.000 0.000 0.000 0.001 0.001
## [12] 0.000 0.001 0.000 0.000 0.000 0.000 0.001 0.000 0.000 0.000 0.000
## [23] 0.000 0.000 0.000
## 
## $resample
## named list()
## 
## $models
## $models$`5`
## Learner model for id=regr.km class=regr.km
## Trained on obs: 25
## Used features: 1
## Hyperparameters: jitter=FALSE,covtype=matern3_2
## 
## 
## $multipoint.lcb.lambdas
##      [,1]
## 
## attr(,"class")
## [1] "MBOResult"
```

