library(mlrMBO)
library(mco)
library(emoa)

## The fabulous lz1-function to test the optimization
lz1 = function(x){
  f = numeric(2)
  n = length(x)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum((x[J1] - x[1]^(0.5 * (1 + 3 * (J1 - 2) / (n - 2) )))^2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum((x[J2] - x[1]^(0.5 * (1 + 3 * (J2 - 2) / (n - 2) )))^2)
  return(f)
}

par.set = makeParamSet(
  makeNumericVectorParam("x", len = 5, lower = 0, upper = 1)
  )
learner = makeLearner("regr.km")
control = makeMBOControl(number.of.targets = 2L,
                         iters = 20, parEGO.s = 100L)
funs = list(fun1 = function(x) zdt1(x$x),
            fun2 = function(x) lz1(x$x))
for(fun in funs) {
  resMBO = mboParEGO(fun, par.set, learner  =learner, control = control)
  plot(resMBO$pareto.front, xlim = c(0, 2), ylim = c(0, 2))
  curve(1 - sqrt(x), add = TRUE)
  resNSGA2 = nsga2(zdt1, 5, 2, lower.bounds=rep(0, 30), upper.bounds=rep(1, 30),
    popsize = 20, generations = 2)
  points(resNSGA2$value, pch = 6)
}