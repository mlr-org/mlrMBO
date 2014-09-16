library(BatchExperiments)
library(mco)
library(emoa)
library(devtools)
library(soobench)

source("bench/mco/testfunctionsMultiObjective.R")
source("bench/mco/defs.R")

unlink("mco_bench-files", recursive = TRUE)
reg = makeExperimentRegistry("mco_bench", packages = c(
    "mco",
    "emoa",
    "mlrMBO"
  ), src.files = c(
    "bench/mco/defs.R",
    "bench/mco/testfunctionsMultiObjective.R", 
    "bench/mco/testfunctionsSingleObjective.R"
  )
)

addMyProblem = function(id, objective, lower, upper, dim_x, dim_y) {
  addProblem(reg, id = id, static = list(
      objective = objective,
      par.set = makeNumericParamSet("x", len = dim_x, lower = lower, upper = upper),
      ny = dim_y,
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
addMyProblem("GOMOP2D2M", GOMOP2D2M, lower = 0, upper = 1, dim_x = 2L, dim_y = 2L)
addMyProblem("GOMOP5D2M", GOMOP5D2M, lower = 0, upper = 1, dim_x = 5L, dim_y = 2L)
addMyProblem("GOMOP2D5M", GOMOP2D5M, lower = 0, upper = 1, dim_x = 2L, dim_y = 5L)
addMyProblem("GOMOP5D5M", GOMOP5D5M, lower = 0, upper = 1, dim_x = 5L, dim_y = 5L)
for (i in c(1:3)) {
  fname = sprintf("zdt%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1, dim_x = 5L, dim_y = 2L)
}
addMyProblem("dtlz1", dtlz1, lower = 0, upper = 1, dim_x = 5L, dim_y = 5L)
addMyProblem("dtlz2_5D2M", dtlz2_5D2M, lower = 0, upper = 1, dim_x = 5L, dim_y = 2L)
addMyProblem("dtlz2_5D5M", dtlz2_5D5M, lower = 0, upper = 1, dim_x = 5L, dim_y = 5L)



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

batchExport(reg, runMBO = runMBO)
# submitJobs(reg, ids = chunk(getJobIds(reg), n.chunks = 33, shuffle = TRUE),
  # resources=list(walltime=8*3600, memory=2*1024),
  # wait=function(retries) 1, max.retries=10)
