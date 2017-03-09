#####################################################
###
### optimizing a simple sin(x) with multipoint proposal
###
#####################################################
\dontrun{
library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeSingleObjectiveFunction(
  name = "Sine",
  fn = function(x) sin(x),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1L),
  global.opt.value = -1
)

ctrl = makeMBOControl(propose.points = 2L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritMeanResponse())
ctrl = setMBOControlMultiPoint(
	ctrl,
  method = "moimbo",
  moimbo.objective = "ei.dist",
  moimbo.dist = "nearest.neighbor",
  moimbo.maxit = 200L
)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(4L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn,
  control = ctrl, points.per.dim = 100, show.info = TRUE)

print(run)

plotExampleRun(run, densregion = TRUE, gg.objects = list(theme_bw()))
}
