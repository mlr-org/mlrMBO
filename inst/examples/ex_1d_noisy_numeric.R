##### optimizing a simple noisy sin(x) with mbo / EI

library(mlrMBO)
library(ggplot2)
load_all()
set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

# function with noise
obj.fun = makeSingleObjectiveFunction(
  name = "Some noisy function",
  fn = function(x) sin(x) + rnorm(1, 0, 0.1),
  par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1L),
  noisy = TRUE
)

# here in this example we know the true, deterministic function
obj.fun.mean = function(x) {
  sin(x$x)
}

ctrl = makeMBOControl(
  init.design.points = 6L,
  iters = 5L,
  propose.points = 1L,
  final.method = "best.predicted",
  final.evals = 10L
)

lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)

ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch",
  opt.focussearch.points = 500L)


run = exampleRun(obj.fun, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 200L, noisy.evals = 50L, fun.mean = obj.fun.mean,
  show.info = TRUE)

print(run)

plotExampleRun(run, pause = pause, densregion = TRUE)
