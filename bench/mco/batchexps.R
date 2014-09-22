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

addMyProblem = function(id, objective, lower, upper, dimx, dimy, prob.seed) {
  addProblem(reg, id = id, seed = prob.seed, static = list(
      objective = objective,
      par.set = makeNumericParamSet("x", len = dimx, lower = lower, upper = upper),
      dimx = dimx,
      dimy = dimy
  ), dynamic = function(static) {
    list(
      design = generateDesign(n = INIT_DESIGN_POINTS(static$dimx), par.set = static$par.set)
    )
  })
}

# test functions
addMyProblem("GOMOP_2D2M", GOMOP_2D2M, lower = 0, upper = 1, dimx = 2L, dimy = 2L, prob.seed =  1)
addMyProblem("GOMOP_5D2M", GOMOP_5D2M, lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  2)
addMyProblem("GOMOP_2D5M", GOMOP_2D5M, lower = 0, upper = 1, dimx = 2L, dimy = 5L, prob.seed =  3)
addMyProblem("GOMOP_5D5M", GOMOP_5D5M, lower = 0, upper = 1, dimx = 5L, dimy = 5L, prob.seed =  4)
addMyProblem("zdt1_5D2M",  zdt1,       lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  5)
addMyProblem("zdt2_5D2M",  zdt2,       lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  6)
addMyProblem("zdt3_5D2M",  zdt3,       lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  7)
addMyProblem("dtlz1_5D5M", dtlz1_5D5M, lower = 0, upper = 1, dimx = 5L, dimy = 5L, prob.seed =  8)
addMyProblem("dtlz2_5D2M", dtlz2_5D2M, lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  9)
addMyProblem("dtlz2_5D5M", dtlz2_5D5M, lower = 0, upper = 1, dimx = 5L, dimy = 5L, prob.seed = 10)

runMBO = function(static, dynamic, method, crit, opt, prop.points, indicator = "sms") {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  learner = makeLearner("regr.km", predict.type = "se")
  iters = MBO_ITERS(static$dimx, prop.points)

  ctrl = makeMBOControl(number.of.targets = static$dimy,
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
  ctrl = setMBOControlMultiCrit(ctrl, method = method,
    ref.point.method = MULTICRIT_REFPOINT, ref.point.offset = MULTICRIT_REFPOINT_OFFSET,
    dib.indicator = indicator, dib.sms.eps = DIB_SMS_EPS,
    parego.rho = PAREGO_RHO, parego.sample.more.weights = PAREGO_SAMPLE_MORE_WEIGHTS
  )

  res = mbo(makeMBOFunction(static$objective), static$par.set, design = dynamic$design,
    learner = learner, control = ctrl, show.info = TRUE)
  list(par.set = par.set, opt.path = opt.path, opt.res = res, mbo.control = control)
}

addAlgorithm(reg, "nsga2", fun = function(static, budget) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  opt.path = makeOptPathDF(par.set, paste("y", 1:static$dimy, sep = "_"),
    minimize = rep(TRUE, static$dimy), include.error.message = TRUE, include.exec.time = TRUE)

  gens = if (budget == "normal")
    BASELINE_NSGA2_GENERATIONS1(static$dimx)
  else
    BASELINE_NSGA2_GENERATIONS2(static$dimx)

  res = nsga2(static$objective, idim = static$dimx,
    odim = static$dimy, lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = BASELINE_NSGA2_POPSIZE(static$dimx), generations = 1:gens)
  # add all elements to op.path
  lapply(seq_along(res), function(i) {
    r = res[[i]]
    for (j in seq_row(r$par))
      addOptPathEl(opt.path, x = list(x = r$par[j, ]), y = r$value[j, ], dob = i)
  })
  list(par.set = par.set, opt.path = opt.path, opt.res = res)
})

addAlgorithm(reg, "parego", fun = function(static, dynamic, prop.points) {
  runMBO(static, dynamic, "parego", PAREGO_CRIT, "focussearch", prop.points)
})

addAlgorithm(reg, "dib", fun = function(static, dynamic, prop.points, indicator) {
  runMBO(static, dynamic, "dib", "dib", "focussearch", prop.points, indicator = indicator)
})

addAlgorithm(reg, "mspot", fun = function(static, dynamic, prop.points, crit) {
  runMBO(static, dynamic, "mspot", crit, "nsga2", prop.points)
})

des1 = makeDesign("nsga2", exhaustive = list(
  budget = c("normal", "10fold")
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

# submitJobs(reg, ids = chunk(getJobIds(reg), n.chunks = 250L, shuffle = TRUE),
 # resources = list(walltime = 1*3600, memory = 2*1024),
 # wait = function(retries) 1, max.retries = 10)
