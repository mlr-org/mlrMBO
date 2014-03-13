#library(devtools)
#load_all
library(parallelMap)
library(mlrMBO)
library(mco)
library(emoa)

par.set = makeParamSet(
  makeNumericVectorParam("x", len = 5, lower = 0, upper = 1)
  )
learner = makeLearner("regr.km", predict.type = "se")
control = makeMBOControl(number.of.targets = 2L, iters = 5L, parEGO.s = 100L,
  parEGO.propose.points = 2L)
funs = list(fun1 = function(x) { zdt1(x$x)},
            fun2 = function(x) lz1(x$x))
parallelStartSocket(cpus = 2)
parallelLibrary("mco")
parallelSource("todo-files/runParEgoSourceFiles.R")
for(fun in funs[2]) {
  resMBO = mboParEGO(fun, par.set, learner = learner, control = control)
  plot(resMBO$pareto.front, xlim = c(0, 2), ylim = c(0, 2))
  curve(1 - sqrt(x), add = TRUE)
}
parallelStop()

