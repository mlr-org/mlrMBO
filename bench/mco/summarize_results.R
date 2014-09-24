library(BatchExperiments)
library(parallelMap)
library(plyr)

reg = loadRegistry("mco_bench-files", work.dir = ".")

prob.ids = getProblemIds(reg)
# parallelStartBatchJobs(bj.resources = list(memory = 4000))
# parallelLibrary("BatchExperiments", "emoa", "ParamHelpers")
# merged.fronts = parallelMap(function(pid, reg) {
#   messagef("Merging front: %s", pid)
#   ids = findExperiments(reg, prob.pattern = pid, match.substring = FALSE)
#   xs = loadResults(reg, ids)
#   fronts = lapply(xs, function(x) getOptPathParetoFront(x$opt.path))
#   merged = do.call(rbind, fronts)
#   t(nondominated_points(t(merged)))
# }, prob.ids, use.names = TRUE, more.args = list(reg = reg))
# parallelStop()

merged.fronts.min = sapply(merged.fronts, function(y) apply(y, 2, min), simplify = FALSE)
merged.fronts.max = sapply(merged.fronts, function(y) apply(y, 2, max), simplify = FALSE)

res = reduceResultsExperimentsParallel(reg, 1:500, njobs = 50L,
  fun = function(job, res, merged.fronts, merged.fronts.min, merged.fronts.max) {

  library(ParamHelpers)
  library(emoa)
  op = as.data.frame(res$opt.path)
  p = getOptPathParetoFront(res$opt.path)
  dimy = ncol(p)
  n.front = nrow(p)

  # scale front + merged front. NB: apply transposes....
  y.min = merged.fronts.min[[job$prob.id]]
  y.max = merged.fronts.max[[job$prob.id]]
  myscale = function(y) {
    t(apply(y, 1, function(r) {
      1 + (r - y.min) / (y.max - y.min)
    }))
  }
  p = myscale(p)
  refset = merged.fronts[[job$prob.id]]
  refset = myscale(refset)

  ref = rep(2.1, dimy)
  ideal = rep(1, dimy)
  nadir = rep(2, dimy)

  sumCounter = function(counter)
   if (job$algo.id %nin% c("randomSearch", "nsga2")) sum(counter) else 0

  list(
    front.size = n.front,
    errmod = sumCounter(op$error.model),
    filter = sumCounter(op$filter.proposed),
    random = sumCounter(!is.na(op$error.model) | isTRUE(op$filter.proposed)),
    hv = hypervolume_indicator(t(p), t(refset), ref = ref),
    eps = epsilon_indicator(t(p), t(refset)),
    r2 = r2_indicator(t(p), t(refset), ideal = ideal, nadir = nadir, lambda = 2L)
  )
}, merged.fronts = merged.fronts, merged.fronts.min = merged.fronts.min, merged.fronts.max = merged.fronts.max)

aggr = ddply(res, getResultVars(res), summarise,
  front.size = mean(front.size),
  errmod = mean(errmod),
  filter = mean(filter),
  random = mean(random),
  hv = mean(hv),
  eps = mean(eps),
  r2 = mean(r2)
)

save2(file = "results.RData", res, aggr, merged.fronts, merged.fronts.min, merged.fronts.max)

