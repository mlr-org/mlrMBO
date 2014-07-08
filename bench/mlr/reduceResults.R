library(BBmisc)
library(BatchExperiments)
library(plyr)

reg = loadRegistry("modelsel-files")

res1 = reduceResultsExperiments(reg, fun = function(job, res) res$aggr)

res2 = reduceResultsExperiments(reg, fun = function(job, res)
  c(as.list(res$aggr), res$extract[[1]]$x))

aggr = ddply(res1, setdiff(getResultVars(res1), "iter"), summarise, err = mean(mmce.test.mean))

save2(res1 = res1, res2 = res2, aggr = aggr, file = "results.RData")

