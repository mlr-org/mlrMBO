library(BatchExperiments)
library(mco)
library(emoa)
library(devtools)

source("DTLZ.R")
source("LZ09.R")
source("UF.R")
source("WFG.R")
source("ZDT.R")

source("defs.R")

unlink("mco_bench-files", recursive = TRUE)
reg = makeExperimentRegistry("mco_bench", packages = c(
    "mco",
    "emoa",
    "mlrMBO"
  ), src.files = c(
    "defs.R"
  )
)

addMyProblem = function(id, objective, lower, upper) {
  addProblem(reg, id = id,, static = list(
      objective = objective,
      par.set = makeNumericParamSet("x", len = 5L, lower = lower, upper = upper),
      ny = 2L,
      ref = c(1, 1)
  ))
}

# test functions
for (i in 1:6) {
  fname = sprintf("dtlz%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}
for (i in c(1:5, 7:9)) {
  fname = sprintf("lz%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}
for (i in 4:7) {
  fname = sprintf("uf%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}
for (i in 1:9) {
  fname = sprintf("wfg%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}
for (i in c(1:4, 6)) {
  fname = sprintf("zdt%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}



addAlgorithm(reg, "nsga2", fun = function(static) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  res = nsga2(static$objective, idim = getParamNr(par.set, devectorize = TRUE),
    odim = static$ny, lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = POPSIZE, generations = GENERATIONS)
  pareto.set = setColNames(as.data.frame(res$par[res$pareto.optimal, ]), names.x)
  pareto.front = res$value[res$pareto.optimal, ]
  hv = dominated_hypervolume(t(pareto.front), ref = static$ref)
  cd = crowding_distance(t(pareto.front))
  r2 = unary_r2_indicator(t(pareto.front), weights = WEIGHTS)
  list(pareto.set = pareto.set, pareto.front = pareto.front, hv = hv, cd = cd, r2 = r2)
})

addAlgorithm(reg, "parego", fun = function(static) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  learner = makeLearner("regr.km", predict.type = "se")

  ctrl = makeMBOControl(number.of.targets = static$ny, init.design.points = INIT_DESIGN_POINTS,
    iters = ITERS, propose.points = PROP_POINTS,
    save.on.disk.at = integer(0L))
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 100, opt.restarts = 1L)
  ctrl = setMBOControlMultiCrit(ctrl)

  res = mbo(makeMBOFunction(static$objective), static$par.set,
    learner = learner, control = ctrl, show.info = TRUE)
  hv = dominated_hypervolume(t(res$pareto.front), ref = static$ref)
  cd = crowding_distance(t(res$pareto.front))
  r2 = unary_r2_indicator(t(res$pareto.front), weights = WEIGHTS)
  list(paretoset = res$pareto.set, pareto.front = res$pareto.front, hv = hv, cd = cd, r2 = r2)
})


addExperiments(reg, repls = REPLS)

testJob(reg, 11, external = TRUE)

# submitJobs(reg, resources=list(walltime=8*3600, memory=2*1024),
#   wait=function(retries) 1, max.retries=10)

