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
      par.set = makeNumericParamSet("x", len = DIM_X, lower = lower, upper = upper),
      ny = DIM_Y,
      ref = REFERENCE_POINT,
      makeResult = function(set, front, ref) {
        hv = dominated_hypervolume(t(front), ref = ref)
        cd = crowding_distance(t(front))
        r2 = unary_r2_indicator(t(front), weights = WEIGHTS)
        list(pareto.set = set, pareto.front = front, hv = hv, cd = cd, r2 = r2)
      }
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


runMBO = function(static, method, crit, opt, prop.points) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  learner = makeLearner("regr.km", predict.type = "se")
  iters = (FEVALS - INIT_DESIGN_POINTS) / prop.points

  ctrl = makeMBOControl(number.of.targets = static$ny, init.design.points = INIT_DESIGN_POINTS,
    iters = iters, propose.points = prop.points,
    save.on.disk.at = integer(0L))
  ctrl = setMBOControlInfill(ctrl, crit = crit, opt = opt,
    opt.focussearch.points = FOCUSSEARCH_POINTS,
    opt.restarts = FOCUSSEARCH_RESTARTS,
    opt.focussearch.maxit = FOCUSSEARCH_MAXIT,
    opt.nsga2.generations = MSPOT_NSGA2_GENERATIONS,
    opt.nsga2.popsize = MSPOT_NSGA2_POPSIZE
  )
  ctrl = setMBOControlMultiCrit(ctrl, method = method)

  res = mbo(makeMBOFunction(static$objective), static$par.set,
    learner = learner, control = ctrl, show.info = TRUE)
  static$makeResult(res$pareto.set, res$pareto.front, static$ref)
}


addAlgorithm(reg, "nsga2", fun = function(static, generations) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  res = nsga2(static$objective, idim = getParamNr(par.set, devectorize = TRUE),
    odim = static$ny, lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = BASELINE_NSGA2_POPSIZE, generations = generations)
  pareto.set = setColNames(as.data.frame(subset(res$par, res$pareto.optimal, drop = FALSE)),
    names.x)
  pareto.front = subset(res$value, res$pareto.optimal, drop = FALSE)
  static$makeResult(pareto.set, pareto.front, static$ref)
})

addAlgorithm(reg, "parego", fun = function(static, prop.points) {
  runMBO(static, "parego", "ei", "focussearch", prop.points)
})

addAlgorithm(reg, "sms", fun = function(static, prop.points) {
  runMBO(static, "sms", "sms", "focussearch", prop.points)
})

addAlgorithm(reg, "mspot", fun = function(static, prop.points) {
  runMBO(static, "mspot", "ei", "nsga2", prop.points)
})


des1 = makeDesign("nsga2", exhaustive = list(
  generations = c(BASELINE_NSGA2_GENERATIONS1, BASELINE_NSGA2_GENERATIONS2)
))
des2 = makeDesign("parego", exhaustive = list(
  prop.points = c(1L, PARALLEL_PROP_POINTS)
))
des3 = makeDesign("sms", exhaustive = list(
  prop.points = c(1L)
))
des4 = makeDesign("mspot", exhaustive = list(
  prop.points = c(1L)
))


addExperiments(reg, algo.design = des1, repls = REPLS)
addExperiments(reg, algo.design = des2, repls = REPLS)
addExperiments(reg, algo.design = des3, repls = REPLS)
addExperiments(reg, algo.design = des4, repls = REPLS)

batchExport(reg, runMBO)
# submitJobs(reg, ids = chunk(getJobIds(reg), n.chunks = 33, shuffle = TRUE),
  # resources=list(walltime=8*3600, memory=2*1024),
  # wait=function(retries) 1, max.retries=10)
