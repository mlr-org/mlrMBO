# MULTIFID BATCH JOBS
library(checkmate)
library(mlr)
library(mlrMBO)
library(BatchExperiments)
library(ggplot2)
library(gridExtra)
library(foreign)
source("todo-files/multifid/benchmark/helpers.R")
source("todo-files/multifid/benchmark/giveMe.R")
source("todo-files/multifid/benchmark/mbo_batchmark.R")

BatchJobs::loadConfig(conffile = "../.BatchJobs.R")
e.resources = list(walltime = 8*60^2)

e.string = paste0("bJ_regression_",format(Sys.time(), "%Y_%m%d_%H%M"))
dir.create(paste0("../plots/", e.string), showWarnings = FALSE)

budget = 100000L
exec.time.budget = 10*60
time.budget = 7*60^2
init.design.points = 30L
tasks = giveMeTasks(c("kin8nm", "puma32H"))
resampling.inner = giveMeResampleDesc("inner")
resampling.outer = giveMeResampleDesc("cv")
learners = giveMeLearners(c("regr.svm"))
tune.controls = giveMeTuneControls(budget = budget, exec.time.budget = exec.time.budget, time.budget = time.budget, init.design.points = init.design.points)
tuned.learners = giveMeTunedLearners(learners = learners, tune.controls = tune.controls, rsi = resampling.inner, measures = mse)
resamplings = replicate(n = length(tasks), resampling.outer, simplify = FALSE)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))

reg = makeExperimentRegistry(e.string, packages = c("mlr", "mlrMBO"))
batchmark(reg, learners = tuned.learners, tasks = tasks, resamplings, measures = list(mse, timetrain), overwrite = TRUE, repls = 1, save.opt.result = TRUE)
#rres = testJob(reg, 1, external = FALSE, resources = list(walltime = 60L))
submitJobs(reg, sample(getJobIds(reg)), resources = e.resources)
waitForJobs(reg)
all.res = reduceResultsList(reg = reg)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))

#####
##### Visualisierung ####
#####

giveMeVisuals(all.res, e.string, init.design.points = tune.controls[[1]]$mbo.control$init.design.points, tasks = tasks)

# save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))
