library(BatchExperiments)
library(mco)
library(emoa)
load_all("..")

unlink("mco_bench-files", recursive = TRUE)
reg = makeExperimentRegistry("mco_bench", packages = c(
    "mco",
    "emoa",
    "mlrMBO"
))

addMyProblem = function(id, objective, lower, upper) {
  addProblem(reg, id = id,, static = list(
      objective = objective,
      par.set = makeNumericParamSet("x", len = 5L, lower = lower, upper = upper),
      ny = 2L,
      ref = c(1, 1)
  ))
}

for (i in 1:3) {
  fname = sprintf("zdt%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}
for (i in 1:10) {
  fname = sprintf("UF%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}

# add more testfunctions


addAlgorithm(reg, "nsga2", fun = function(static) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  res = nsga2(static$objective, idim = getParamNr(par.set, devectorize = TRUE), odim = static$ny,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = 60L, generations = 10L)
  pareto.set = setColNames(as.data.frame(res$par[res$pareto.optimal, ]), names.x)
  pareto.front = res$value[res$pareto.optimal, ]
  hv = dominated_hypervolume(t(pareto.front), ref = static$ref)
  # add crowding dist and r2 indic
  list(pareto.set = pareto.set, pareto.front = pareto.front, hv = hv)
})

addAlgorithm(reg, "parego", fun = function(static) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  learner = makeLearner("regr.km", predict.type = "se")

  ctrl = makeMBOControl(number.of.targets = static$ny, init.design.points = 10L, iters = 10)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 100, opt.restarts = 1L)
  ctrl = setMBOControlMulticrit(ctrl)

  res = mbo(makeMBOFunction(static$objective), static$par.set,
    learner = learner, control = ctrl, show.info = TRUE)
  hv = dominated_hypervolume(t(res$pareto.front), ref = static$ref)
  # add crowding dist and r2 indic
  list(paretoset = res$pareto.set, pareto.front = res$pareto.front, hv = hv)
})


addExperiments(reg)

testJob(reg, 2, external = FALSE)

