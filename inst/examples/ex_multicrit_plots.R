library(mlrMBO)
library(smoof)
library(ggplot2)

set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeDTLZ1Function(dimensions = 5L, n.objectives = 2L)

lrn = makeLearner("regr.km", predict.type = "se")
ctrl = makeMBOControl(n.objectives = 2L,
  propose.points = 2L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 1000L,
  opt.focussearch.maxit = 3L)
ctrl = setMBOControlMultiCrit(ctrl, method = "parego")

design = generateDesign(8L, getParamSet(obj.fun), fun = lhs::maximinLHS)

res = mbo(obj.fun, design = design, learner = lrn, control = ctrl, show.info = TRUE)

plot(res, pause = pause)
