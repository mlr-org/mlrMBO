#####################################################
###
### optimizing multi-objective function
###
#####################################################
\dontrun{
library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeZDT1Function(dimensions = 2L)

ctrl = makeMBOControl(n.objectives = 2L, propose.points = 2L, save.on.disk.at = integer(0L))
ctrl = setMBOControlTermination(ctrl, iters = 5L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritDIB(),
  opt.focussearch.points = 10000L)
ctrl = setMBOControlMultiObj(ctrl, parego.s = 100)
learner = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(5L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRunMultiObj(obj.fun, design = design, learner = learner, ctrl, points.per.dim = 50L,
  show.info = TRUE, nsga2.args = list())

plotExampleRun(run, gg.objects = list(theme_bw()))
}
