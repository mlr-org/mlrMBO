##### optimizing branin in 2D with multipoint proposal #####

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(ggplot2)
library(grid)
library(gridExtra)

load_all(".", reset=TRUE)

configureMlr(show.learner.output=FALSE)

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

run = exampleRun(obj.fun, learner = lrn, control = ctrl, points.per.dim = 50)
print(run)

res = autoplot(run, pause = FALSE)

