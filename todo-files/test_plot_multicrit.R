library(mco)
library(devtools)
library(checkmate)
load_all()
source("todo-files/plot_multicritMBO.R")
f = makeMBOFunction(zdt1)
ps = makeNumericParamSet(len = 5L, lower = 0, upper = 1)

set.seed(1)
# Test normal run
learner = makeLearner("regr.km", predict.type = "se")
ctrl = makeMBOControl(iters = 10L, n.objectives = 2L, init.design.points = 8L,
  propose.points = 2L)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 1000L,
  opt.focussearch.maxit = 3L)
ctrl = setMBOControlMultiCrit(ctrl, method = "parego")#, dib.indicator = "eps")
or = mbo(f, ps, learner = learner, control = ctrl)
plot(or, alpha = TRUE, infill.crit = "ei", log.infill.crit = FALSE, pause = TRUE)
