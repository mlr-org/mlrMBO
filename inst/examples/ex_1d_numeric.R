#####################################################
###
### optimizing a simple sin(x) with mbo / EI
###
#####################################################
\dontrun{
library(ggplot2)
library(mlrMBO)
configureMlr(show.learner.output = FALSE)
set.seed(1)

obj.fun = makeSingleObjectiveFunction(
  name = "Sine",
  fn = function(x) sin(x),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1),
  global.opt.value = -1
)

ctrl = makeMBOControl(propose.points = 1)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), 
  opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn,
  control = ctrl, points.per.dim = 100, show.info = TRUE)

plotExampleRun(run, densregion = TRUE, gg.objects = list(theme_bw()))
}
