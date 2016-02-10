##### optimizing a simple sin(x) with mbo / EI #####

#library(mlrMBO)
library(ggplot2)
library(mlrMBO)
library(smoof)
configureMlr(show.learner.output = FALSE)
pause = interactive()
set.seed(1)

obj.fun = makeSingleObjectiveFunction(
  name = "Sine",
  fn = function(x) sin(x),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1)
)

ctrl = makeMBOControl(init.design.points = 6, iters = 10, propose.points = 1)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(obj.fun, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100, show.info = TRUE)

plotYTraces(list(run = run$mbo.res$opt.path))
plotExampleRun(run, pause = pause, densregion = TRUE)
