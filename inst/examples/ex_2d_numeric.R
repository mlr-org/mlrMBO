#####################################################
###
### optimizing branin in 2D with mbo / EI #####
###
#####################################################
\dontrun{
library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeBraninFunction()

ctrl = makeMBOControl(propose.points = 1L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), 
  opt = "focussearch", opt.focussearch.points = 2000L)

lrn = makeMBOLearner(ctrl, obj.fun)
design = generateDesign(10L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn, control = ctrl,
	points.per.dim = 50L, show.info = TRUE)

print(run)

plotExampleRun(run, gg.objects = list(theme_bw()))
}
