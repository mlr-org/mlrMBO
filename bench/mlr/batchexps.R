library(BBmisc)
library(lhs)
library(BatchExperiments)
library(fail)
# load_all("~/cos/mlr")
# load_all("~/cos/mlrMBO")


unlink("modelsel-files", recursive = TRUE)
reg = makeExperimentRegistry("modelsel", packages = c("mlr"), src.files = c(
  "defs.R",
  "multiplexer.R",
  "trainPredict.R",
  "algoTemplate.R"
))

fhandle = fail("data")

# load data, create task and set pos class as min class,
# then remove const features and create CV instance
myAddProblem = function(reg, id, data, target) {
  y = data[, target]
  # remove crappy special chars in colnames
  cns = colnames(data)
  j = which(cns != target)
  cns[j] = sprintf("v%i", seq_along(j))
  colnames(data) = cns
  data = droplevels(data)
  data = removeConstantFeatures(data)
  task = makeClassifTask(data = data, target = target)
  rdesc = makeResampleDesc("CV", iters = CV_FOLDS_OUTER, stratify = TRUE)
  rin = makeResampleInstance(rdesc, task = task)
  addProblem(reg, id = id, static = list(
    task = task,
    rin = rin
  ))
}

fs = fhandle$ls()
# fs = fs[1]
for (f in fs) {
  messagef("Adding data set: %s", f)
  myAddProblem(reg, id = f, data = fhandle$get(f), "class")
}

source("algos.R")

addMyExps = function(id) {
  des = makeDesign(id, exhaustive = list(
    iter = 1:CV_FOLDS_OUTER)
  )
  addExperiments(reg, algo.designs = des)
}

addMyExps("random")
# addMyExps("irace")
addMyExps("mbo")

#print(reg)
#getJob(reg, 1)
# testJob(reg, 1, external = FALSE)

# submitJobs(reg)
# waitForJobs(reg)
# res = reduceResultsExperiments(reg, fun = function(job, res) res$aggr)
# print(res)
#testJob(reg, , external = TRUE)
