
library(mlrMBO)
library(ggplot2)
set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

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
  has.simple.signature = FALSE
)

ctrl = makeMBOControl(init.design.points = 4L, iters = 10L, propose.points = 1L)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch", opt.focussearch.points = 500L)

lrn = makeLearner("regr.randomForest", predict.type = "se")

run = exampleRun(obj.fun, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100L, show.info = TRUE)

print(run)

plotExampleRun(run, pause = pause, densregion = TRUE)
