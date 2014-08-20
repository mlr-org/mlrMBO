##### optimizing a simple noisy sin(x) with mbo / EI

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(ggplot2)
library(grid)
library(gridExtra)

load_all(".", reset = TRUE)

configureMlr(show.learner.output = FALSE)

set.seed(1)

# function with noise
obj.fun = function(x) {
  sin(x$x) + rnorm(1, 0, 0.1)
}

# here in this example we know the true, deterministic function
obj.fun.mean = function(x) {
  sin(x$x)
}

par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1L)

ctrl = makeMBOControl(
  init.design.points = 6L,
  iters = 5L,
  propose.points = 1L,
  final.method = "best.predicted",
  final.evals = 10L,
  noisy = TRUE
)

lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)

ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch",
  opt.focussearch.points = 500L)


run = exampleRun(obj.fun, par.set, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 200L, noisy.evals = 50L, fun.mean = obj.fun.mean)

print(run)

res = autoplot(run, pause = TRUE, densregion = TRUE)
