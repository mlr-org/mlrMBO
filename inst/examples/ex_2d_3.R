##### optimizing a 2d function with both factor variable
##### and numeric variable with random forest and ei

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

configureMlr(show.learner.output = TRUE)

objfun = function(x) {
  if (x$foo == "a")
    sum(x$x^2)
  else if (x$foo == "b")
    sum(x$x^2) + 10
  else
    sum(x$x^2) - 10
}

par.set = makeParamSet(
  makeDiscreteParam("foo", values = letters[1:3]),
  makeNumericVectorParam("x", len = 1, lower = -2, upper = 3)
)

lrn = makeLearner("regr.randomForest", predict.type = "se")

ctrl = makeMBOControl(init.design.points = 6, iters = 10, propose.points = 1)
ctrl = setMBOControlInfill(ctrl, crit = "ei")

run = exampleRun(objfun, par.set = par.set, global.opt = -10, learner = lrn,
  control = ctrl, points.per.dim = 100)

print(run)

#autoplot(run, pause=TRUE, densregion=FALSE)






