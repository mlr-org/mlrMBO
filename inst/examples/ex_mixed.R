
library(mlr)
library(soobench)
library(ggplot2)
library(grid)
library(gridExtra)

set.seed(1)

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

ctrl = makeMBOControl(init.design.points = 4, iters = 10, propose.points = 1)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeLearner("regr.randomForest", predict.type = "se")

run = exampleRun(obj.fun, par.set, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100)

print(run)

res = autoplot(run, pause = TRUE, densregion = TRUE)
