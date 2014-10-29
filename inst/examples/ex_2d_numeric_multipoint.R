##### optimizing branin in 2D with multipoint proposal #####

library(mlrMBO)
library(ggplot2)
library(soobench)
set.seed(2) # FIXME: does not work for seed == 1
configureMlr(show.learner.output = FALSE)

obj.fun = branin_function()

par.set = extractParamSetFromSooFunction(obj.fun)

ctrl = makeMBOControl(init.design.points = 10, iters = 10, propose.points = 5)
ctrl = setMBOControlMultiPoint(ctrl,
  method = "multicrit",
  multicrit.objective = "ei.dist",
  multicrit.dist = "nearest.neighbor",
  multicrit.maxit = 200
)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(obj.fun, par.set = par.set, learner = lrn, control = ctrl,
	points.per.dim = 50, show.info = TRUE)

print(run)

res = autoplot(run, pause = TRUE)
