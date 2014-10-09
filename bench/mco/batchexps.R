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
    dimy = dimy,
    id = id
  ), dynamic = function(static) {
    list(
      design = generateDesign(n = INIT_DESIGN_POINTS(static$dimx), par.set = static$par.set)
    )
  })
}

# test functions
addMyProblem("GOMOP_2D2M",  GOMOP_2D2M,  lower = 0, upper = 1, dimx = 2L, dimy = 2L, prob.seed =  1)
addMyProblem("GOMOP_5D2M",  GOMOP_5D2M,  lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  2)
addMyProblem("GOMOP_2D5M",  GOMOP_2D5M,  lower = 0, upper = 1, dimx = 2L, dimy = 5L, prob.seed =  3)
addMyProblem("GOMOP_5D5M",  GOMOP_5D5M,  lower = 0, upper = 1, dimx = 5L, dimy = 5L, prob.seed =  4)
addMyProblem("zdt1_5D2M",   zdt1,        lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  5)
addMyProblem("zdt2_5D2M",   zdt2,        lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  6)
addMyProblem("zdt3_5D2M",   zdt3,        lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  7)
addMyProblem("dtlz1_5D5M",  dtlz1_5D5M,  lower = 0, upper = 1, dimx = 5L, dimy = 5L, prob.seed =  8)
addMyProblem("dtlz2_5D2M",  dtlz2_5D2M,  lower = 0, upper = 1, dimx = 5L, dimy = 2L, prob.seed =  9)
addMyProblem("dtlz2_5D5M",  dtlz2_5D5M,  lower = 0, upper = 1, dimx = 5L, dimy = 5L, prob.seed = 10)
addMyProblem("GOMOP2_2D3M", GOMOP2_2D3M, lower = 0, upper = 1, dimx = 2L, dimy = 3L, prob.seed = 11)
addMyProblem("GOMOP3_3D2M", GOMOP3_3D2M, lower = 0, upper = 1, dimx = 3L, dimy = 2L, prob.seed = 12)

runMBO = function(static, dynamic, method, crit, opt, prop.points, indicator = "sms") {
  learner = makeLearner("regr.km", covtype = "matern5_2", predict.type = "se")
  iters = MBO_ITERS(static$dimx, prop.points)

  ctrl = makeMBOControl(number.of.targets = static$dimy,
    iters = iters, propose.points = prop.points,
    save.on.disk.at = integer(0L))
  ctrl = setMBOControlInfill(ctrl, crit = crit, opt = opt,
    filter.proposed.points = FILTER_PROPOSED_POINTS,
    filter.proposed.points.tol = FILTER_PROPOSED_POINTS_TOL,
    crit.lcb.lambda = NULL, crit.lcb.pi = LCB_PI,
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
  list(par.set = static$par.set, opt.path = res$opt.path, opt.res = res, mbo.control = ctrl)
}

addAlgorithm(reg, "randomSearch", fun = function(static, dynamic, budget) {
  opt.path = makeOptPathDF(static$par.set, paste("y", 1:static$dimy, sep = "_"),
    minimize = rep(TRUE, static$dimy), include.error.message = TRUE, include.exec.time = TRUE)

  budget = if (budget == "normal") BASELINE_RANDOMSEARCH_BUDGET1 else BASELINE_RANDOMSEARCH_BUDGET2
  # sample new points
  newdes = sampleValues(static$par.set, budget(static$dimx), discrete.names = FALSE, trafo = FALSE)
  newdes = extractSubList(newdes, "x", simplify = "rows")
  # add to design + eval + add to opt.path
  design = rbind(as.matrix(dynamic$design), newdes)
  ys = t(apply(design, 1, static$objective))
  for (i in seq_row(design))
    addOptPathEl(opt.path, x = list(x = design[i, ]), y = ys[i, ], dob = 0L)
  list(par.set = static$par.set, opt.path = opt.path)
})

addAlgorithm(reg, "nsga2", overwrite = TRUE, fun = function(static, budget) {
  opt.path = makeOptPathDF(static$par.set, paste("y", 1:static$dimy, sep = "_"),
    minimize = rep(TRUE, static$dimy), include.error.message = TRUE, include.exec.time = TRUE)

  gens = if (budget == "normal") BASELINE_NSGA2_GENERATIONS1 else BASELINE_NSGA2_GENERATIONS2
  
  dimx = static$dimx
  res = nsga2(static$objective, idim = dimx,
    odim = static$dimy, lower.bounds = getLower(static$par.set), upper.bounds = getUpper(static$par.set),
    popsize = BASELINE_NSGA2_POPSIZE(dimx), generations = 1:gens(dimx),
    cprob = BASELINE_NSGA2_cprob(dimx), cdist = BASELINE_NSGA2_cdist(dimx), 
    mprob = BASELINE_NSGA2_mprob(dimx), mdist = BASELINE_NSGA2_mdist(dimx))
  # add all elements to op.path
  lapply(seq_along(res), function(i) {
    r = res[[i]]
    for (j in seq_row(r$par))
      addOptPathEl(opt.path, x = list(x = r$par[j, ]), y = r$value[j, ], dob = i)
  })
  list(par.set = static$par.set, opt.path = opt.path, opt.res = res)
})

# Add NSGA2 with really high number of FEvals to calc the "exact" front
addAlgorithm(reg, "exactFront", overwrite = TRUE, fun = function(static) {
  opt.path = makeOptPathDF(static$par.set, paste("y", 1:static$dimy, sep = "_"),
    minimize = rep(TRUE, static$dimy), include.error.message = TRUE, include.exec.time = TRUE)  
  dimx = static$dimx
  res = nsga2(static$objective, idim = dimx,
    odim = static$dimy, lower.bounds = getLower(static$par.set), upper.bounds = getUpper(static$par.set),
    popsize = EXACT_NSGA2_POPSIZE(dimx), generations =EXACT_NSGA2_GENERATIONS(dimx),
    cprob = BASELINE_NSGA2_cprob(dimx), cdist = BASELINE_NSGA2_cdist(dimx), 
    mprob = BASELINE_NSGA2_mprob(dimx), mdist = BASELINE_NSGA2_mdist(dimx))
  # add all elements to op.path
  for (j in seq_row(res$par))
    addOptPathEl(opt.path, x = list(x = res$par[j, ]), y = res$value[j, ], dob = i)
  list(par.set = static$par.set, opt.path = opt.path, opt.res = res)
})

addAlgorithm(reg, "parego", fun = function(static, dynamic, prop.points, crit) {
  runMBO(static, dynamic, "parego", crit, "focussearch", prop.points)
})

addAlgorithm(reg, "dib", fun = function(static, dynamic, prop.points, indicator) {
  runMBO(static, dynamic, "dib", "dib", "focussearch", prop.points, indicator = indicator)
})

addAlgorithm(reg, "mspot", fun = function(static, dynamic, prop.points, crit) {
  runMBO(static, dynamic, "mspot", crit, "nsga2", prop.points)
})

des1 = makeDesign("randomSearch", exhaustive = list(
  budget = c("normal", "10fold")
))
des2 = makeDesign("nsga2", exhaustive = list(
  budget = c("normal", "10fold")
))
des3 = makeDesign("parego", exhaustive = list(
  prop.points = PARALLEL_PROP_POINTS,
  crit = c("lcb", "ei")
))
des4 = makeDesign("dib", exhaustive = list(
  prop.points = PARALLEL_PROP_POINTS,
  indicator = c("sms", "eps")
))
des5 = makeDesign("mspot", exhaustive = list(
  prop.points = PARALLEL_PROP_POINTS,
  crit = c("mean", "lcb", "ei")
))
des6 = makeDesign("exactFront")

addExperiments(reg, algo.design = des1, repls = REPLS)
addExperiments(reg, algo.design = des2, repls = REPLS)
addExperiments(reg, algo.design = des3, repls = REPLS)
addExperiments(reg, algo.design = des4, repls = REPLS)
addExperiments(reg, algo.design = des5, repls = REPLS)

batchExport(reg, runMBO = runMBO)

j = findExperiments(reg, algo.pattern = "nsga2")

submitJobs(reg, ids = j,
  resources = list(walltime = 8 * 3600, memory = 2 * 1024),
  wait = function(retries) 60, max.retries = 10)
