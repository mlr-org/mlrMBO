library(BatchExperiments)
library(soobench)
library(ParamHelpers)
load_all("~/cos/mlrMBO")

unlink("soo_bench-files", recursive = TRUE)
reg = makeExperimentRegistry("soo_bench", packages = c(
    "ParamHelpers",
    "soobench",
    "mlrMBO"
  ), src.files = c(
    "defs.R"
  )
)

addProblem(reg, id = "bbob", dynamic = function(d, fid) {
  objective = bbob2009_function(d, fid = fid, iid = 1L)
  par.set = makeNumericParamSet("x", len = d,
    lower = lower_bounds(objective), upper = upper_bounds(objective))
  list(objective = objective, par.set = par.set, d = d)
})


# addAlgorithm(reg, "gensa", fun = function(dynamic) {
#   f = dynamic$objective
#   par.set = dynamic$par.set
#   start = sampleValue(par.set)$x
#   ctrl = list(simple.function = FALSE, smooth = FALSE, max.call = BUDGET(dynamic$d))
#   res = GenSA(par = start, fn = f, control = ctrl,
#     lower = getLower(par.set), upper = getUpper(par.set))
#   gap = res$value - global_minimum(f)$value
#   list(x = res$par, y = res$value, gap = gap, count =  res$count, trace = res$trace.mat)
# })

# addAlgorithm(reg, "bfgs", fun = function(dynamic) {
#   f = dynamic$objective
#   par.set = dynamic$par.set
#   start = sampleValue(par.set)$x
#   ctrl = list()
#   res = optim(par = start, fn = f, method = "L-BFGS-B", control = ctrl,
#     lower = getLower(par.set), upper = getUpper(par.set))
#   gap = res$value - global_minimum(f)$value
#   list(x = res$par, y = res$value, gap = gap, count =  res$count)
# })

addAlgorithm(reg, "bfgs", fun = function(dynamic) {
  f = dynamic$objective
  par.set = dynamic$par.set
  start = sampleValue(par.set)$x
  ctrl = list()
  res = optim(par = start, fn = f, method = "L-BFGS-B", control = ctrl,
    lower = getLower(par.set), upper = getUpper(par.set))
  gap = res$value - global_minimum(f)$value
  list(x = res$par, y = res$value, gap = gap, count =  res$count)
})

addAlgorithm(reg, "mbo", fun = function(dynamic) {
  d = dynamic$d
  f = dynamic$objective
  par.set = dynamic$par.set

  surrogate = makeLearner(MBO_SURROGATE)
  surrogate = setHyperPars(surrogate, par.vals = MBO_SURROGATE_VALS)
  surrogate = setPredictType(surrogate, "se")

  ctrl = makeMBOControl(init.design.points = MBO_INIT_DES_SIZE(d), iters = MBO_ITERS(d))
  ctrl = setMBOControlInfill(ctrl, crit = MBO_INFILL_CRIT,
    opt.restarts = MBO_FOCUSSEARCH_RESTARTS,
    opt.focussearch.maxit = MBO_FOCUSSEARCH_ITERS,
    opt.focussearch.points = MBO_FOCUSSEARCH_POINTS
  )

  res = mbo(makeMBOFunction(f), par.set, learner = surrogate, control = ctrl)
  gap = res$y - global_minimum(f)$value
  list(x = res$x, y = res$y, gap = gap, opt.path = as.data.frame(res$opt.path))
})

pdes = makeDesign("bbob", exhaustive = list(
  d = c(2, 5),
  fid = 1:24
))

addExperiments(reg, prob.designs = pdes)

z = testJob(reg, 2, external = FALSE)


