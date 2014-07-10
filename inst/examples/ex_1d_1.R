##### optimizing a simple sin(x) with mbo / EI #####

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

obj.fun = function(x) {
  sin(x$x)
}

par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1)

ctrl = makeMBOControl(init.design.points = 6, iters = 10, propose.points = 1)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(obj.fun, par.set, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100)

print(run)
res = autoplot(run, pause = FALSE, densregion = TRUE)
