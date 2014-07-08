library(BBmisc)
library(BatchExperiments)
library(plyr)

reg = loadRegistry("soo_bench-files")

res = reduceResultsExperiments(reg, fun = function(job, res) res[c("y", "gap")])

aggr = ddply(res, getResultVars(res), summarise, y = mean(y), gap = mean(gap))

save2(res = res, aggr = aggr, file = "results.RData")


