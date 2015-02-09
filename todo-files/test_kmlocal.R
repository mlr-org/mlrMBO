library(mlr)
library(DiceOptim)
library(ggplot2)
library(devtools)
load_all()
source("todo-files/local_krig.R")

set.seed(1)

f = branin2
d = 2L
ps = makeNumericParamSet("x", len = d, lower = 0, upper = 1)

# n = 200
# x = generateDesign(n, ps)
# y = apply(x, 1, f)
# d = cbind(as.data.frame(x), y = y)
#
# task = makeRegrTask(data = d, target = "y")
# lrn = makeLearner("regr.kmlocal", predict.type = "se")
# m = train(lrn, task)
# p = predict(m, task)
#
# mm = m$learner.model
#

surrogate = makeLearner("regr.kmlocal", predict.type = "se")

ctrl = makeMBOControl(init.design.points = 8L, iters = 32L, on.learner.error = "stop")
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "cmaes", opt.restarts = 3L)

res = mbo(makeMBOFunction(f), par.set = ps, learner = surrogate, control = ctrl)
op = as.data.frame(res$opt.path)
mm = res$models[[1L]]$learner.model

ggdata = op
ggdata$isc = FALSE
ggdata$isc[mm$local.centers.i] = TRUE

pl = ggplot(ggdata, aes(x = x1, y = x2, col = isc))
pl = pl + geom_point(size = 3)
print(pl)