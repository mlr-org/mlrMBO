library(mlrMBO)
library(smoof)

set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeZDT1Function(dimensions = 5L)

lrn = makeLearner("regr.km", predict.type = "se")
ctrl = makeMBOControl(iters = 10L, number.of.targets = 2L, init.design.points = 8L,
  propose.points = 2L)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 1000L,
  opt.focussearch.maxit = 3L)
ctrl = setMBOControlMultiCrit(ctrl, method = "parego")

res = mbo(obj.fun, learner = lrn, control = ctrl, show.info = TRUE)

plot(res, pause = pause)
