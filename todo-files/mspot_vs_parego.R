library(emoa)
f = makeMBOFunction(zdt1)
ps = makeNumericParamSet(len = 5L, lower = 0, upper = 1)

# Test normal run
learner = makeLearner("regr.km", predict.typ = "se")
ctrl = makeMBOControl(iters = 20, number.of.targets = 2L, init.design.points = 10L)
ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10000, crit = "ei")
ctrl = setMBOControlMultiCrit(ctrl, method = "parego")
or.parego = mbo(f, ps, learner = learner, control = ctrl)

ctrl = setMBOControlMultiCrit(ctrl, method = "mspot")
or.mspot = mbo(f, ps, learner = learner, control = ctrl)
plot(or.parego$pareto.front)
points(or.mspot$pareto.front, pch = 3)
