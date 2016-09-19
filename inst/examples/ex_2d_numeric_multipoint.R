##### optimizing branin in 2D with multipoint proposal #####

library(mlrMBO)
library(ggplot2)
library(smoof)
set.seed(2) # FIXME: does not work for seed == 1
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeBraninFunction()

ctrl = makeMBOControl(propose.points = 5L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlMultiPoint(ctrl,
  method = "multicrit",
  multicrit.objective = "ei.dist",
  multicrit.dist = "nearest.neighbor",
  multicrit.maxit = 200L
)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

design = generateDesign(10L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn, control = ctrl,
	points.per.dim = 50L, show.info = TRUE)

print(run)

plotExampleRun(run, pause = pause, gg.objects = list(theme_bw()))
