
library(mlrMBO)
library(ggplot2)
library(mco)

set.seed(1)
configureMlr(show.learner.output = FALSE)

# f = makeMBOFunction(function(x) {
  # c(x^2, (x - 2)^2)
# })
f = makeMBOFunction(zdt1)
ps = makeNumericParamSet(len = 2L, lower = 0, upper = 1)

learner = makeLearner("regr.km", nugget.estim = FALSE,
  predict.type = "se")

iters = 10L
ctrl = makeMBOControl(iters = iters, number.of.targets = 2L,
  init.design.points = 5L, save.on.disk.at = integer(0L))
ctrl = setMBOControlInfill(ctrl, crit = "dib",
  opt.focussearch.points = 10000)
ctrl = setMBOControlMultiCrit(ctrl, parego.s = 100)

run = exampleRunMultiCrit(f, ps, learner, ctrl, points.per.dim = 50,
  show.info = TRUE, nsga2.args = list(), ref.point = c(11, 11))

autoplot(run, pause = TRUE, iters = 1:iters)
