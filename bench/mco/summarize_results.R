library(BatchExperiments)
library(parallelMap)
library(plyr)
library(mlrMBO)

reg = loadRegistry("mco_bench-files", work.dir = ".")

# exclude GOMOP_2D5M for now - ref.front too big
prob.ids = setdiff(getProblemIds(reg), "GOMOP_2D5M")

job.ids = getJobIds(reg)
#nsga2.10fold.ids = findExperiments(reg, algo.pattern = "nsga2", algo.pars = budget == "10fold")
#rs.10fold.ids = findExperiments(reg, algo.pattern = "randomSearch", algo.pars = budget == "10fold")
job.ids = setdiff(job.ids, union(nsga2.10fold.ids, rs.10fold.ids))
job.ids = setdiff(job.ids, findExperiments(reg, prob.pattern = "GOMOP_2D5M"))

parallelStartBatchJobs(bj.resources = list(memory = 4000))
parallelLibrary("BatchExperiments", "emoa", "ParamHelpers")
merged.fronts = parallelMap(function(pid, reg, job.ids) {
  messagef("Merging front: %s", pid)
  ids = findExperiments(reg, ids = job.ids, prob.pattern = pid, match.substring = FALSE)
  xs = loadResults(reg, ids)
  fronts = lapply(xs, function(x) getOptPathParetoFront(x$opt.path))
  merged = do.call(rbind, fronts)
  t(nondominated_points(t(merged)))
}, prob.ids, use.names = TRUE, more.args = list(reg = reg, job.ids = job.ids))
parallelStop()

merged.fronts.min = sapply(merged.fronts, function(y) apply(y, 2, min), simplify = FALSE)
merged.fronts.max = sapply(merged.fronts, function(y) apply(y, 2, max), simplify = FALSE)

# Pre-Calc Referenz Front HV and R2 to save some time
parallelStartBatchJobs(bj.resources = list(memory = 4000))
parallelLibrary("BatchExperiments", "emoa", "ParamHelpers")
referenz.front.hvr2 = parallelMap(function(merged.front, y.min, y.max) {
  myscale = function(y) {
    t(apply(y, 1, function(r) {
      1 + (r - y.min) / (y.max - y.min)
    }))
  }
  merged.front = myscale(merged.front)
  dimy = ncol(merged.front)
  ref = rep(2.1, dimy)
  ideal = rep(1, dimy)
  s = switch(dimy, 1L, 100000L, 450L, 75L, 37L)
  weight.vectors = mlrMBO:::combWithSum(s, dimy) / s
  
  list(
    ref.hv = dominated_hypervolume(t(merged.front), ref),
    ref.r2 = unary_r2_indicator(t(merged.front), t(weight.vectors), ideal = ideal)
  )
}, merged.front = merged.fronts, y.min = merged.fronts.min,
  y.max = merged.fronts.max, use.names = TRUE)
parallelStop()
names(referenz.front.hvr2) = prob.ids

res = reduceResultsExperimentsParallel(reg, job.ids, njobs = 50L,
  fun = function(job, res, merged.fronts, merged.fronts.min, merged.fronts.max, referenz.front.hvr2) {
    
    library(ParamHelpers)
    library(emoa)
    op = as.data.frame(res$opt.path)
    p = getOptPathParetoFront(res$opt.path)
    dimy = ncol(p)
    n.front = nrow(p)
    
    # scale front + merged front. NB: apply transposes....
    y.min = merged.fronts.min[[job$prob.id]]
    y.max = merged.fronts.max[[job$prob.id]]
    hv.r2 = referenz.front.hvr2[[job$prob.id]]
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
    s = switch(dimy, 1L, 100000L, 450L, 75L, 37L)
    weight.vectors = mlrMBO:::combWithSum(s, dimy) / s
    
    sumCounter = function(counter)
      if (job$algo.id %nin% c("randomSearch", "nsga2")) sum(counter) else 0
    p.hv = dominated_hypervolume(t(p), ref)
    p.r2 = unary_r2_indicator(t(p), t(weight.vectors), ideal = ideal)
    
    list(
      front.size = n.front,
      errmod = sumCounter(is.na(op$error.model)),
      filter = sumCounter(op$filter.proposed),
      random = sumCounter(!is.na(op$error.model) | isTRUE(op$filter.proposed)),
      hv = hv.r2$ref.hv - p.hv,
      eps = epsilon_indicator(t(p), t(refset)),
      r2 = p.r2 - hv.r2$ref.r2
    )
  },
  merged.fronts = merged.fronts, merged.fronts.min = merged.fronts.min,
  merged.fronts.max = merged.fronts.max, referenz.front.hvr2 = referenz.front.hvr2)

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

