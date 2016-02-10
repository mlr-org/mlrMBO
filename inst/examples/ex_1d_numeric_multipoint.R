##### optimizing a simple sin(x) with multipoint proposal #####

library(mlrMBO)
library(ggplot2)
library(smoof)
set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeSingleObjectiveFunction(
  name = "Sine",
  fn = function(x) sin(x),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1L)
)

ctrl = makeMBOControl(init.design.points = 4L, iters = 10L, propose.points = 2L)
ctrl = setMBOControlMultiPoint(
	ctrl,
  	method = "multicrit",
  	multicrit.objective = "ei.dist",
  	multicrit.dist = "nearest.neighbor",
  	multicrit.maxit = 200L
)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(obj.fun, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100, show.info = TRUE)

print(run)
plotYTraces(list(run = run$mbo.res$opt.path))
plotExampleRun(run, pause = pause, densregion = TRUE)
