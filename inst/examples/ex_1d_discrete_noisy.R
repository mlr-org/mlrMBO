##### optimizing 1D fun with 3 categorical level and
##### noisy outout with random forest

library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

objfun = function(x) {
  if (x$foo == "a") {
    return(5 + rnorm(1))
  } else if (x$foo == "b") {
    return(4 + rnorm(1, sd = 0.5))
  } else {
    return(3 + rnorm(1, sd = 1))
  }
}

par.set = makeParamSet(makeDiscreteParam("foo", values = letters[1:3]))

ctrl = makeMBOControl(init.design.points = 20, iters = 1, noisy = TRUE)
# we can basically do an exhaustive search in 3 values
ctrl = setMBOControlInfill(ctrl, crit = "ei",
  opt.restarts = 1L, opt.focussearch.points = 3L, opt.focussearch.maxit = 1L)

lrn = makeLearner("regr.randomForest", predict.type = "se")

run = exampleRun(objfun, par.set = par.set, learner = lrn, control = ctrl,
	points.per.dim = 50, show.info = TRUE)

print(run)
plotExampleRun(run, pause = pause, densregion = TRUE)
