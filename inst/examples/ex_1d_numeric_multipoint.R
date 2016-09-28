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
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1L),
  global.opt.value = -1
)

ctrl = makeMBOControl(propose.points = 2L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)

ctrl = setMBOControlMultiPoint(
	ctrl,
  	method = "multicrit",
  	multicrit.objective = "ei.dist",
  	multicrit.dist = "nearest.neighbor",
  	multicrit.maxit = 200L
)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

design = generateDesign(4L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn,
  control = ctrl, points.per.dim = 100, show.info = TRUE)

print(run)

plotExampleRun(run, pause = pause, densregion = TRUE, gg.objects = list(theme_bw()))
