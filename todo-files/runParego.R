library(mlrMBO)
library(mco)
library(emoa)

par.set = makeParamSet(
  makeNumericVectorParam("x", len = 5, lower = 0, upper = 1)
  )
learner = makeLearner("regr.km")
control = makeMBOControl(number.of.targets = 2L, iters = 5L, parEGO.s = 100L,
  parEGO.multipoint.number = 20L, parEGO.source.file = "todo-files/runParEgoSourceFiles.R")
funs = list(fun1 = function(x) { zdt1(x$x)},
            fun2 = function(x) { lz1(x$x)})
parallelStartSocket(cpus = 2)
for(fun in funs[1]) {
  resMBO = mboParEGO(fun, par.set, learner = learner, control = control)
  plot(resMBO$pareto.front, xlim = c(0, 2), ylim = c(0, 2))
  curve(1 - sqrt(x), add = TRUE)
}
parallelStop()