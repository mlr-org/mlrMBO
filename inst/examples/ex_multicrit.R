library(mlrMBO)
library(ggplot2)
library(smoof)

set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeZDT1Function(dimensions = 2L)

learner = makeLearner("regr.km", nugget.estim = FALSE, predict.type = "se")

ctrl = makeMBOControl(n.objectives = 2L, propose.points = 2L, save.on.disk.at = integer(0L))
ctrl = setMBOControlTermination(ctrl, iters = 5L)
ctrl = setMBOControlInfill(ctrl, crit = "dib",
  opt.focussearch.points = 10000L)
ctrl = setMBOControlMultiCrit(ctrl, parego.s = 100)

design = generateDesign(5L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRunMultiCrit(obj.fun, design = design, learner = learner, ctrl, points.per.dim = 50L,
  show.info = TRUE, nsga2.args = list())

plotExampleRun(run, pause = pause, gg.objects = list(theme_bw()))
