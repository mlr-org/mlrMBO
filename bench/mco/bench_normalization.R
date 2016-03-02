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
  "defs.R",
  "WFG.R"
  )
)

addMyProblem = function(id, objective, lower, upper) {
  addProblem(reg, id = id,, static = list(
    objective = objective,
    par.set = makeNumericParamSet("x", len = 5L, lower = lower, upper = upper),
    ny = 2L,
    ref = c(11, 11)
  ))
}

# test functions
for (i in 1:9) {
  fname = sprintf("wfg%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1)
}

addAlgorithm(reg, "parego", fun = function(static, normalization) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  learner = makeLearner("regr.km", predict.type = "se")
  iters = (FEVALS - INIT_DESIGN_POINTS)

  ctrl = makeMBOControl(n.objectives = static$ny, init.design.points = INIT_DESIGN_POINTS,
    iters = iters, propose.points = 1L,
    save.on.disk.at = integer(0L))
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 10000L,
    opt.restarts = 3L, opt.focussearch.maxit = 3L)
  ctrl = setMBOControlMultiCrit(ctrl, parego.normalize = normalization)

  res = mbo(makeMBOFunction(static$objective), static$par.set,
    learner = learner, control = ctrl, show.info = TRUE)
  hv = dominated_hypervolume(t(res$pareto.front), ref = static$ref)
  cd = crowding_distance(t(res$pareto.front))
  r2 = unary_r2_indicator(t(res$pareto.front), weights = WEIGHTS)
  list(paretoset = res$pareto.set, pareto.front = res$pareto.front, hv = hv, cd = cd, r2 = r2)
})

des = makeDesign("parego", exhaustive = list(
  normalization = c("standard", "front")
))

addExperiments(reg, algo.design = des, repls = 10L)

submitJobs(reg, ids = getJobIds(reg),
  resources=list(walltime=1*3600, memory=2*1024),
  wait=function(retries) 1, max.retries=10)
showStatus(reg)
