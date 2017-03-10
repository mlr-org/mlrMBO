#####################################################
###
### optimizing mixed space function
###
#####################################################
\dontrun{
library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)

obj.fun = makeSingleObjectiveFunction(
  name = "Mixed functions",
  fn = function(x) {
    if (x$cat == "a")
      x$num^2
    else
      x$num^2 + 3
  },
  par.set = makeParamSet(
    makeDiscreteParam("cat", values = c("a", "b")),
    makeNumericParam("num", lower = -5, upper = 5)
  ),
  has.simple.signature = FALSE,
  global.opt.value = -1
)

ctrl = makeMBOControl(propose.points = 1L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), 
  opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeMBOLearner(ctrl, obj.fun)

design = generateDesign(4L, getParamSet(obj.fun), fun = lhs::maximinLHS)

run = exampleRun(obj.fun, design = design, learner = lrn,
  control = ctrl, points.per.dim = 100L, show.info = TRUE)

print(run)

plotExampleRun(run, densregion = TRUE, gg.objects = list(theme_bw()))
}
