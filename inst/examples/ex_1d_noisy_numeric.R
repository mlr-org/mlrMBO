#####################################################
###
### optimizing a simple noisy sin(x) with mbo / EI
###
#####################################################
\dontrun{
library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)

# function with noise
obj.fun = makeSingleObjectiveFunction(
  name = "Some noisy function",
  fn = function(x) sin(x) + rnorm(1, 0, 0.1),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1L),
  noisy = TRUE,
  global.opt.value = -1,
  fn.mean = function(x) sin(x)
)

ctrl = makeMBOControl(
  propose.points = 1L,
  final.method = "best.predicted",
  final.evals = 10L
)
ctrl = setMBOControlTermination(ctrl, iters = 5L)

ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
 opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(6L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn,
  control = ctrl, points.per.dim = 200L, noisy.evals = 50L,
  show.info = TRUE)

print(run)

plotExampleRun(run, densregion = TRUE, gg.objects = list(theme_bw()))
}
