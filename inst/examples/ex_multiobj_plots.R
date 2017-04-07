#####################################################
###
### optimizing multi objective function and plots
###
#####################################################
\dontrun{
library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeDTLZ1Function(dimensions = 5L, n.objectives = 2L)

ctrl = makeMBOControl(n.objectives = 2L,
  propose.points = 2L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), opt.focussearch.points = 1000L,
  opt.focussearch.maxit = 3L)
ctrl = setMBOControlMultiObj(ctrl, method = "parego")
lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(8L, getParamSet(obj.fun), fun = lhs::maximinLHS)

res = mbo(obj.fun, design = design, learner = lrn, control = ctrl, show.info = TRUE)

plot(res)
}
