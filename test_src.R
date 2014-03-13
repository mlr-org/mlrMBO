library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(mco)

load_all(".", reset=TRUE)

set.seed(6)


par.set = makeNumericParamSet(len = 2L, lower = 0, upper = 1)
learner = makeLearner("regr.km", predict.type = "se")
control = makeMBOControl(number.of.targets = 2L, iters = 2L, parEGO.s = 10L, init.design.points = 5,
  parEGO.propose.points = 2L)

resMBO = mboParEGO(makeMBOFunction(zdt1), par.set, learner = learner, control = control)


# r  plot(resMBO$pareto.front, xlim = c(0, 2), ylim = c(0, 2))
  # curve(1 - sqrt(x), add = TRUE)
# }
# parallelStop()

