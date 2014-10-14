library(BatchExperiments)
library(parallelMap)
library(plyr)
library(mlrMBO)

reg = loadRegistry("mco_bench-files", work.dir = ".")

# exclude dtlz1_5D5M - results don't show anything
prob.ids = setdiff(getProblemIds(reg), c("dtlz1_5D5M"))

job.ids = getJobIds(reg)
# exclude the rs, nsga2-10fold and dtlz2_5D5M
# it's a "bug/feature" in BatchExperiments - nsga2-ref ids are included above
# so this is a bit annoying to code ...
exactFront.ids = findExperiments(reg, algo.pattern = "nsga2-ref")
nsga2.ids  = setdiff(findExperiments(reg, algo.pattern = "nsga2"), exactFront.ids)
nsga2.10fold.ids = findExperiments(reg, algo.pattern = "nsga2")
rs.ids = findExperiments(reg, algo.pattern = "randomSearch")
job.ids = setdiff(job.ids, union(nsga2.10fold.ids, union(rs.ids, exactFront.ids)))
job.ids = setdiff(job.ids, findExperiments(reg, prob.pattern = "dtlz1_5D5M"))

parallelStartBatchJobs(bj.resources = list(memory = 20000))
parallelLibrary("BatchExperiments", "emoa", "ParamHelpers")
merged.fronts = parallelMap(function(pid, reg, job.ids) {
  messagef("Merging front: %s", pid)
  ids = findExperiments(reg, ids = job.ids, prob.pattern = pid, match.substring = FALSE)
  xs = loadResults(reg, ids)
  fronts = lapply(xs, function(x) getOptPathParetoFront(x$opt.path))
  merged = do.call(rbind, fronts)
  list(merged.front = merged, ref.front = t(nondominated_points(t(merged))))
}, prob.ids, use.names = TRUE, more.args = list(reg = reg, job.ids = job.ids))
parallelStop()

merged.fronts.min = sapply(merged.fronts, function(y) apply(y$merged.front, 2, min), simplify = FALSE)
merged.fronts.max = sapply(merged.fronts, function(y) apply(y$merged.front, 2, max), simplify = FALSE)

reduceFunction = function(job, res, merged.fronts, merged.fronts.min, merged.fronts.max, indic) {
  library(ParamHelpers)
  library(emoa)
  op = as.data.frame(res$opt.path, include.x = FALSE, include.rest = FALSE)
  dimy = ncol(op)
  # scale merged front. NB: apply transposes....
  y.min = merged.fronts.min[[job$prob.id]]
  y.max = merged.fronts.max[[job$prob.id]]
  myscale = function(y) {
    t(apply(y, 1, function(r) {
      1 + (r - y.min) / (y.max - y.min)
    }))
  }
  refset = merged.fronts[[job$prob.id]]$ref.front
  refset = myscale(refset)
  ref = rep(2.1, dimy)
  ideal = rep(1, dimy)
  s = switch(dimy, 1L, 100000L, 450L, 75L, 37L)
  weight.vectors = mlrMBO:::combWithSum(s, dimy) / s
  
  # calculate the indicators for every dob
  dob = getOptPathDOB(res$opt.path)
  r = lapply(unique(dob), function(i) {
    p = myscale(getOptPathParetoFront(res$opt.path, dob = 1:i))
    switch(indic,
      hv = dominated_hypervolume(t(p), ref),
      eps = epsilon_indicator(t(p), t(refset)),
      r2 = unary_r2_indicator(t(p), t(weight.vectors), ideal = ideal))
  })
  names(r) = paste(indic, unique(dob), sep = "")
  r
}

# we have to distinuish 2D and 5D as well as 1 prop.points and 4 prop.points, nsga2 is special, too
ids.2d = intersect(job.ids, findExperiments(reg, prob.pattern = "2D"))
ids.5d = intersect(job.ids, findExperiments(reg, prob.pattern = "5D"))
ids.nsga2 = intersect(job.ids, findExperiments(reg, algo.pattern = "nsga2"))
ids.mmbo = setdiff(job.ids, ids.nsga2)
ids.single = intersect(ids.mmbo, findExperiments(reg, ids.mmbo, algo.pars = prop.points == 1L))
ids.quad = intersect(ids.mmbo, findExperiments(reg, ids.mmbo, algo.pars = prop.points == 4L))

reduceCaller = function(ids, indic)
  reduceResultsExperimentsParallel(reg, ids, njobs = 25L,
    fun = reduceFunction,
    merged.fronts = merged.fronts, merged.fronts.min = merged.fronts.min,
    merged.fronts.max = merged.fronts.max, indic = indic)

res.nsga2 = list()
res.2d.single = list()
res.2d.quad = list()
res.5d.single = list()
res.5d.quad = list()
for (indic in c("hv", "eps", "r2")) {
  res.nsga2[[indic]] = reduceCaller(ids.nsga2, indic)
  res.2d.single[[indic]] = reduceCaller(intersect(ids.2d, ids.single), indic)
  res.2d.quad[[indic]] = reduceCaller(intersect(ids.2d, ids.quad), indic)
  res.5d.single[[indic]] = reduceCaller(intersect(ids.5d, ids.single), indic)
  res.5d.quad[[indic]] = reduceCaller(intersect(ids.5d, ids.quad), indic)
}

save2(file = "results_trends.RData", res.nsga2, res.2d.single, res.2d.quad, res.5d.single, res.5d.quad)
