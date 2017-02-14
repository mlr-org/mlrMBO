##### optimizing branin in 2D with multipoint proposal #####

library(mlrMBO)
library(ggplot2)
library(smoof)
set.seed(2) # FIXME: does not work for seed == 1
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeBraninFunction()

ctrl = makeMBOControl(propose.points = 5L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritMeanResponse())
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlMultiPoint(ctrl,
  method = "moimbo",
  moimbo.objective = "ei.dist",
  moimbo.dist = "nearest.neighbor",
  moimbo.maxit = 200L
)

#lrn = makeMBOLearner(ctrl, obj.fun)
#FIXME: Remove lrn after #314 is fixed
lrn = makeLearner("regr.km", predict.type = "se")
design = generateDesign(10L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn, control = ctrl,
	points.per.dim = 50L, show.info = TRUE)

print(run)

plotExampleRun(run, pause = pause, gg.objects = list(theme_bw()))
