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



addAlgorithm(reg, "nsga2", fun = function(static, generations) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  res = nsga2(static$objective, idim = getParamNr(par.set, devectorize = TRUE),
    odim = static$ny, lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = POPSIZE, generations = generations)
  pareto.set = setColNames(as.data.frame(subset(res$par, res$pareto.optimal, drop = FALSE)), 
    names.x)
  pareto.front = subset(res$value, res$pareto.optimal, drop = FALSE)
  hv = dominated_hypervolume(t(pareto.front), ref = static$ref)
  cd = crowding_distance(t(pareto.front))
  r2 = unary_r2_indicator(t(pareto.front), weights = WEIGHTS)
  list(pareto.set = pareto.set, pareto.front = pareto.front, hv = hv, cd = cd, r2 = r2)
})

addAlgorithm(reg, "parego", fun = function(static, prop.points) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  learner = makeLearner("regr.km", predict.type = "se")
  iters = (FEVALS - INIT_DESIGN_POINTS) / prop.points

  ctrl = makeMBOControl(number.of.targets = static$ny, init.design.points = INIT_DESIGN_POINTS,
    iters = iters, propose.points = prop.points,
    save.on.disk.at = integer(0L))
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 10000L, 
    opt.restarts = 3L, opt.focussearch.maxit = 3L)
  ctrl = setMBOControlMultiCrit(ctrl)

  res = mbo(makeMBOFunction(static$objective), static$par.set,
    learner = learner, control = ctrl, show.info = TRUE)
  hv = dominated_hypervolume(t(res$pareto.front), ref = static$ref)
  cd = crowding_distance(t(res$pareto.front))
  r2 = unary_r2_indicator(t(res$pareto.front), weights = WEIGHTS)
  list(paretoset = res$pareto.set, pareto.front = res$pareto.front, hv = hv, cd = cd, r2 = r2)
})


des1 = makeDesign("nsga2", exhaustive = list(
  generations = c(GENERATIONS1, GENERATIONS2)
))

des2 = makeDesign("parego", exhaustive = list(
  prop.points = c(PROP_POINTS1, PROP_POINTS2)
))

addExperiments(reg, algo.design = des1, repls = REPLS)
addExperiments(reg, algo.design = des2, repls = REPLS)


submitJobs(reg, ids = chunk(getJobIds(reg), n.chunks = 33, shuffle = TRUE),
  resources=list(walltime=8*3600, memory=2*1024),
  wait=function(retries) 1, max.retries=10)
