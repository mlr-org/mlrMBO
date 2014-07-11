# Parallelization

It is possible to parallelize the evaluation of the target function to speed up the computation. Internally the
evaluation of the target function is realized with the R package parallelMap. This package offers a parallel version
of the ```lapply``` function and offers a lot of modes. We cannot review all of them here. The [parallelMap github page](https://github.com/berndbischl/parallelMap#parallelmap) offers a nice tutorial, describes all modes thorougly and
informs you about the available options. Here we just provide a simple example on how to take advantage of multiple cores on a single machine.


```splus
library("mlrMBO")
library("parallelMap")

parallelStartMulticore(cpus = 2) # use 2 CPUs

obj.fun = makeMBOFunction(function(x) sum(x^2))
par.set = makeNumericParamSet(len = 1, id = "x", lower = -10, upper = 10)
learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
control = makeMBOControl(
    init.design.points = 5,
    iters = 3
)
res = mbo(obj.fun, par.set, learner = learner, control = control)

parallelStop()
```
