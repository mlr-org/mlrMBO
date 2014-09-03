##### optimizing branin in 2D with mbo / EI #####

library(BBmisc)
library(mlr)
library(soobench)
library(ggplot2)
library(grid)
library(gridExtra)

configureMlr(show.learner.output = FALSE)

set.seed(423)

obj.fun = branin_function()

par.set = extractParamSetFromSooFunction(obj.fun)

ctrl = makeMBOControl(init.design.points = 10, iters = 10, propose.points = 1)
ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "focussearch", opt.focussearch.points = 2000)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(obj.fun, par.set = par.set, learner = lrn, control = ctrl, points.per.dim = 50)

print(run)

res = autoplot(run, pause = TRUE)
