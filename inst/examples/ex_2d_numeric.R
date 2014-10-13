##### optimizing branin in 2D with mbo / EI #####

library(mlrMBO)
library(ggplot2)
library(soobench)
set.seed(1)
configureMlr(show.learner.output = FALSE)

set.seed(423)

obj.fun = branin_function()

par.set = extractParamSetFromSooFunction(obj.fun)

ctrl = makeMBOControl(init.design.points = 10, iters = 10, propose.points = 1)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch", opt.focussearch.points = 2000)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(obj.fun, par.set = par.set, learner = lrn, control = ctrl, 
	points.per.dim = 50, show.info = TRUE)

print(run)

res = autoplot(run, pause = TRUE)
