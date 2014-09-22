library(plyr)
library(BatchExperiments)

reg = loadRegistry("mco_bench-files")
two.object = c(findExperiments(reg, prob.pattern = "2M"),
  findExperiments(reg, prob.pattern = "zdt"))
five.object = findExperiments(reg, prob.pattern = "5M")

makeGetterFun = function (f) {
  function(job, res) {
    p = getOptPathParetoFront(res)
    mins = apply(p, 2, f)
    mins = as.list(mins)
  }
}

getGlobalExtremum = function(y.extreme, ny, minimize = TRUE) {
  y.names = paste("y", 1:ny, sep = "_")
  if (minimize)
    fun = min
  else
    fun = max
  tapply(1:nrow(y.extreme), y.extreme$prob, function(inds) apply(y.extreme[inds, y.names], 2, fun))
}

y.min.two = reduceResultsExperiments(reg, ids = two.object, fun = makeGetterFun(min))
y.max.two = reduceResultsExperiments(reg, ids = two.object, fun = makeGetterFun(max))
y.min.five = reduceResultsExperiments(reg, ids = five.object, fun = makeGetterFun(min))
y.max.five = reduceResultsExperiments(reg, ids = five.object, fun = makeGetterFun(max))

y.min.two = getGlobalExtremum(y.min.two, 2)
y.max.two = getGlobalExtremum(y.max.two, 2)
y.min.five = getGlobalExtremum(y.min.five, 5)
y.max.five = getGlobalExtremum(y.max.five, 5)

res = reduceResultsExperimentsParallel(reg, fun = function(job, res, y.min.two, y.max.two, y.min.five, y.max.five) {
  library(ParamHelpers)
  library(emoa)
  op = as.data.frame(res)
  # for NSGA2 drop the last 2 generations
  #print(1:(max(getOptPathDOB(res)) - 2))
  if (job$algo.id == "nsga2")
    p = getOptPathParetoFront(res, dob = 1:(max(getOptPathDOB(res)) - 2))
  else
    p = getOptPathParetoFront(res)
  if (ncol(p) == 2) {
    y.min = matrix(rep(y.min.two[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
    y.max = matrix(rep(y.max.two[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
  } else {
    y.min = matrix(rep(y.min.five[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
    y.max = matrix(rep(y.max.five[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
  }
  p = (p - y.min) / (y.max - y.min)
  list(hv = dominated_hypervolume(t(p), ref = rep(2.1, length(res$y.name))),
    front.size = nrow(p), error.model = sum(!is.na(op$error.model)))
}, y.min.two = y.min.two, y.max.two = y.max.two, y.min.five = y.min.five, y.max.five = y.max.five)

aggr = ddply(res, getResultVars(res), summarise, hv = mean(hv), front.size = mean(front.size),
  error.model = mean(error.model))

testfuns = levels(aggr$prob)

getTestFunResults = function(tf) {
  res = subset(aggr, aggr$prob == tf)
  sortByCol(res, "hv", asc = FALSE)
}


