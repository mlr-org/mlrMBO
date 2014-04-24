##### optimizing a simple sin(x) with multipoint proposal #####

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(ggplot2)
library(grid)
library(gridExtra)

load_all(".", reset = TRUE)

configureMlr(show.learner.output = FALSE)

objfun = function(x) {
  sin(x$x)
}

ps = makeNumericParamSet(lower=3, upper = 13, len = 1)

ctrl = makeMBOControl(init.design.points = 4, iters = 10, propose.points = 2,
  multipoint.method = "multicrit",
  multipoint.multicrit.objective = "ei.dist",
  multipoint.multicrit.dist = "nearest.neighbor",
  multipoint.multicrit.maxit = 200
)

lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run = exampleRun(objfun, ps, global.opt = -1, learner = lrn,
  control = ctrl, points.per.dim = 100)

print(run)

autoplot(run, pause = TRUE, densregion = TRUE)
