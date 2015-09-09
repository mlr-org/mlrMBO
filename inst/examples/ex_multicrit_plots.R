library(mco)
library(mlrMBO)

set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeMBOFunction(mco::zdt1)
par.set = makeNumericParamSet(len = 5L, lower = 0, upper = 1)

lrn = makeLearner("regr.km", predict.type = "se")
ctrl = makeMBOControl(iters = 10L, number.of.targets = 2L, init.design.points = 8L,
  propose.points = 2L)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 1000L,
  opt.focussearch.maxit = 3L)
ctrl = setMBOControlMultiCrit(ctrl, method = "parego")

or = mbo(obj.fun, par.set, learner = lrn, control = ctrl, show.info = TRUE)

plot(or, pause = pause)
