library(emoa)
library(mco)
library(devtools)
load_all()
f = makeMBOFunction(zdt1)
ps = makeNumericParamSet(len = 5L, lower = 0, upper = 1)

# Test normal run
learner = makeLearner("regr.km", predict.typ = "se")
ctrl = makeMBOControl(iters = 5L, n.objectives = 2L, init.design.points = 10L)
ctrl = setMBOControlInfill(ctrl, crit = "ei")
ctrl = setMBOControlMultiCrit(ctrl, method = "parego")
or.parego = mbo(f, ps, learner = learner, control = ctrl)

ctrl = setMBOControlMultiCrit(ctrl, method = "sms")
ctrl = setMBOControlInfill(ctrl, crit = "sms")
or.sms = mbo(f, ps, learner = learner, control = ctrl)

ctrl = setMBOControlMultiCrit(ctrl, method = "mspot")
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "nsga2", opt.nsga2.generations = 50L)
or.mspot = mbo(f, ps, learner = learner, control = ctrl)


plot(or.parego$pareto.front)
points(or.sms$pareto.front, pch = 6)
points(or.mspot$pareto.front, pch = 3)
