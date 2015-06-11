library(mlrMBO)
library(mlr)
library(BatchExperiments)
library(checkmate)
library(foreign)
library(dplyr)
reg.old = loadRegistry("bJ_classification_2015_0603_1506-files/")
file.res = load2("../plots/bJ_classification_2015_0603_1506/CV_compare.RData", parts = c("all.res", "tasks"))
all.res.class = file.res$all.res
tasks.class = file.res$tasks
rm(file.res)
job.df = getJobInfo(reg.old)
prob.instances = lapply(job.df$id, function(id) generateProblemInstance(reg = reg.old, id))
save2(file = "prob.instances.RData", prob.instances)
time.cuts = c(30,60,120,240)*60

redoJobGets = function(job, static, dynamic) {
  job.id = dynamic$job.id
  time.cut = dynamic$time.cut
  this.res = static$all.res[[as.character(job.id)]]
  if (is.null(this.res))
    return(NA)
  op.df = as.data.frame(this.res$opt.result$opt.path)
  op.df$exec.time.sum = cumsum(op.df$exec.time)
  min.ind = which.min(subset(op.df, exec.time.sum < time.cut)[,this.res$opt.result$opt.path$y.names])  #FIXME: We ignore .multifid.lvl here
  best.setting = trafoValue(par = this.res$opt.result$opt.path$par.set, getOptPathEl(this.res$opt.result$opt.path, min.ind)$x)
  best.setting = dropNamed(best.setting, ".multifid.lvl")
  lrn = setHyperPars(this.res$opt.result$learner, par.vals = best.setting)
  task = static$tasks[[this.res$task.id]]
  res.inst = makeResampleInstance(desc = makeResampleDesc("Holdout", split = 0.9), task = task)
  res.inst$train.inds[[1]] = static$prob.instance[[job.id]]$train
  res.inst$test.inds[[1]] = static$prob.instance[[job.id]]$test
  res.res = resample(learner = lrn, task = task, resampling = res.inst, measures = lapply(names(this.res$performance), get))
  list(resample.result = res.res, job.id = job.id, time.cut = time.cut)
}

reg = makeExperimentRegistry(id = "redo", packages = "mlr")
addProblem(reg, id = "redo_basic_prob", static = list(tasks = tasks.class, all.res = all.res.class, prob.instances = prob.instances), dynamic = list, overwrite = TRUE)
addAlgorithm(reg, id = "redo_basic_algo", fun = redoJobGets, overwrite = TRUE)
prob.design = makeDesign("redo_basic_prob", exhaustive = list(job.id = job.df$id, time.cut = time.cuts))
addExperiments(reg, algo.designs = "redo_basic_algo", prob.designs = prob.design, skip.defined = TRUE)

testJob(reg, id = 1, external = FALSE)
submitJobs(reg, ids = getJobIds(reg), resources = list(walltime = 60^2), job.delay = TRUE)
waitForJobs(reg)
res = reduceResultsList(reg)
save.image("redo_res_all.RData")
