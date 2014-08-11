
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

obj.fun = function(x) {
  if (x$cat == "a")
    x$num^2
  else
    x$num^2 + 3
}

par.set = makeParamSet(
  makeDiscreteParam("cat", values = c("a", "b")),
  makeNumericParam("num", lower = -5, upper = 5)
)

ctrl = makeMBOControl(init.design.points = 3, iters = 10, propose.points = 1, save.on.disk.at = integer(0L))
ctrl = setMBOControlInfill(ctrl, crit = "mean", opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeLearner("regr.randomForest")

run = exampleRun(obj.fun, par.set, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100)

print(run)

res = autoplot(run, pause = TRUE, densregion = TRUE)

