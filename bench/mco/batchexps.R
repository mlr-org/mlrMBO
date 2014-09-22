library(BatchExperiments)
library(mco)
library(emoa)
library(devtools)
library(soobench)

source("bench/mco/testfunctionsMultiObjective.R")
source("bench/mco/defs.R")
source("bench/mco/testfunctionsSingleObjective.R")

unlink("mco_bench-files", recursive = TRUE)
reg = makeExperimentRegistry("mco_bench", packages = c(
    "mco",
    "emoa",
    "mlrMBO"
  ), src.files = c(
    "bench/mco/defs.R",
    "bench/mco/testfunctionsMultiObjective.R", 
    "bench/mco/testfunctionsSingleObjective.R"
  ),
  seed = 1
)

addMyProblem = function(id, objective, lower, upper, dim_x, dim_y) {
  addProblem(reg, id = id, seed = 1273, static = list(
      objective = objective,
      par.set = makeNumericParamSet("x", len = dim_x, lower = lower, upper = upper),
      nx = dim_x,
      ny = dim_y
  ), dynamic = function(static) {
    design = generateDesign(n = INIT_DESIGN_POINTS * static$nx, par.set = static$par.set)
    list(design = design)
  })
}

# test functions
addMyProblem("GOMOP_2D2M", GOMOP_2D2M, lower = 0, upper = 1, dim_x = 2L, dim_y = 2L)
addMyProblem("GOMOP_5D2M", GOMOP_5D2M, lower = 0, upper = 1, dim_x = 5L, dim_y = 2L)
addMyProblem("GOMOP_2D5M", GOMOP_2D5M, lower = 0, upper = 1, dim_x = 2L, dim_y = 5L)
addMyProblem("GOMOP_5D5M", GOMOP_5D5M, lower = 0, upper = 1, dim_x = 5L, dim_y = 5L)
for (i in 1:3) {
  fname = sprintf("zdt%i", i)
  addMyProblem(fname, get(fname), lower = 0, upper = 1, dim_x = 5L, dim_y = 2L)
}
addMyProblem("dtlz1_5D5M", dtlz1_5D5M, lower = 0, upper = 1, dim_x = 5L, dim_y = 5L)
addMyProblem("dtlz2_5D2M", dtlz2_5D2M, lower = 0, upper = 1, dim_x = 5L, dim_y = 2L)
addMyProblem("dtlz2_5D5M", dtlz2_5D5M, lower = 0, upper = 1, dim_x = 5L, dim_y = 5L)

runMBO = function(static, dynamic, method, crit, opt, prop.points, indicator = "sms") {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  learner = makeLearner("regr.km", predict.type = "se")
  iters = (FEVALS - INIT_DESIGN_POINTS) / prop.points * static$nx

  ctrl = makeMBOControl(number.of.targets = static$ny,
    iters = iters, propose.points = prop.points,
    save.on.disk.at = integer(0L))
  ctrl = setMBOControlInfill(ctrl, crit = crit, opt = opt,
    crit.lcb.lambda = NULL, crit.lcb.PI = LCB_PI,
    opt.focussearch.points = FOCUSSEARCH_POINTS,
    opt.restarts = FOCUSSEARCH_RESTARTS,
    opt.focussearch.maxit = FOCUSSEARCH_MAXIT,
    opt.nsga2.generations = MSPOT_NSGA2_GENERATIONS,
    opt.nsga2.popsize = MSPOT_NSGA2_POPSIZE
  )
  ctrl = setMBOControlMultiCrit(ctrl, method = method, dib.indicator = indicator)

  res = mbo(makeMBOFunction(static$objective), static$par.set, design = dynamic$design,
    learner = learner, control = ctrl, show.info = TRUE)
  return(res$opt.path)
}

addAlgorithm(reg, "nsga2", fun = function(static, generations) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  opt.path = makeOptPathDF(par.set, paste("y", 1:static$ny, sep = "_"),
    minimize = rep(TRUE, static$ny))
  
  res = nsga2(static$objective, idim = static$nx,
    odim = static$ny, lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = BASELINE_NSGA2_POPSIZE * static$nx, generations = 1:generations)
  # add all elements to op.path
  lapply(seq_along(res), function(i) {
    r = res[[i]]
    for (j in seq_row(r$par)) 
      addOptPathEl(opt.path, x = list(x = r$par[j, ]), y = r$value[j, ], dob = i)
  })
  
  return(opt.path)
})

addAlgorithm(reg, "parego", fun = function(static, dynamic, prop.points) {
  runMBO(static, dynamic, "parego", "ei", "focussearch", prop.points)
})

addAlgorithm(reg, "dib", fun = function(static, dynamic, prop.points, indicator) {
  runMBO(static, dynamic, "dib", "dib", "focussearch", prop.points, indicator = indicator)
})

addAlgorithm(reg, "mspot", fun = function(static, dynamic, prop.points, crit) {
  runMBO(static, dynamic, "mspot", crit, "nsga2", prop.points)
})

des1 = makeDesign("nsga2", exhaustive = list(
  generations = c(BASELINE_NSGA2_GENERATIONS1, BASELINE_NSGA2_GENERATIONS2)
))
des2 = makeDesign("parego", exhaustive = list(
  prop.points = PARALLEL_PROP_POINTS
))
des3 = makeDesign("dib", exhaustive = list(
  prop.points = PARALLEL_PROP_POINTS,
  indicator = c("sms", "eps")
))
des4 = makeDesign("mspot", exhaustive = list(
  prop.points = PARALLEL_PROP_POINTS,
  crit = c("mean", "lcb", "ei")
))

addExperiments(reg, algo.design = des1, repls = REPLS)
addExperiments(reg, algo.design = des2, repls = REPLS)
addExperiments(reg, algo.design = des3, repls = REPLS)
addExperiments(reg, algo.design = des4, repls = REPLS)

batchExport(reg, runMBO = runMBO)
 submitJobs(reg, ids = chunk(getJobIds(reg), n.chunks = 250L, shuffle = TRUE),
   resources=list(walltime=1*3600, memory=2*1024),
   wait=function(retries) 1, max.retries=10)
