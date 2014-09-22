library(plyr)
library(BatchExperiments)

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

getGlobalExtremum = function(y.extreme, ny) {
  y.names = paste("y", 1:ny, sep = "_")
  tapply(1:nrow(y.extreme), y.extreme$prob, function(inds) apply(y.extreme[inds, y.names], 2, min))
}

y.min.two = reduceResultsExperiments(reg, ids = two.object, fun = makeGetterFun(min))
y.max.two = reduceResultsExperiments(reg, ids = two.object, fun = makeGetterFun(max))
y.min.five = reduceResultsExperiments(reg, ids = five.object, fun = makeGetterFun(min))
y.max.five = reduceResultsExperiments(reg, ids = five.object, fun = makeGetterFun(max))

y.min.two = getGlobalExtremum(y.min.two, 2)
y.max.two = getGlobalExtremum(y.max.two, 2)
y.min.five = getGlobalExtremum(y.min.five, 5)
y.max.five = getGlobalExtremum(y.max.five, 5)

res = reduceResultsExperiments(reg, fun = function(job, res) {
  p = getOptPathParetoFront(res)
  if (ncol(p) == 2) {
    y.min = matrix(rep(y.min.two[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
    y.max = matrix(rep(y.max.two[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
  } else {
    y.min = matrix(rep(y.min.five[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
    y.max = matrix(rep(y.max.five[[job$prob.id]], nrow(p)), nrow = nrow(p), byrow = TRUE)
  }
  p = (p - y.min) / (y.max - y.min)
  list(hv = dominated_hypervolume(t(p), ref = rep(2.1, length(res$y.name))))
})

aggr = ddply(res, getResultVars(res), summarise, hv = mean(hv))
