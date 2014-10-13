##### optimizing a simple sin(x) with mbo / EI #####

library(mlrMBO)
library(ggplot2)
configureMlr(show.learner.output = FALSE)

set.seed(1)

obj.fun = function(x) {
  sin(x$x)
}

par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1)

ctrl = makeMBOControl(init.design.points = 6, iters = 10, propose.points = 1)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(obj.fun, par.set, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100, show.info = TRUE)

print(run)

res = autoplot(run, pause = TRUE, densregion = TRUE)
