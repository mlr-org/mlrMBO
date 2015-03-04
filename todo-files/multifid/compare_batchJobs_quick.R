# MULTIFID BATCH JOBS
library(checkmate)
library(devtools)
load_all()
library(BatchExperiments)
library(ggplot2)
library(gridExtra)
library(foreign)
source("todo-files/multifid/benchmark/helpers.R")
source("todo-files/multifid/benchmark/giveMe.R")
source("todo-files/multifid/benchmark/mbo_batchmark.R")

e.string = paste0("quicktest")
dir.create(paste0("../plots/", e.string), showWarnings = FALSE)

budget = 20L
tasks = giveMeTasks(c("iris"))
resampling.inner = giveMeResampleDesc("inner")
resampling.outer = giveMeResampleDesc("outer")
learners = giveMeLearners(c("svm"))
tune.controls = giveMeTuneControls(budget)
tuned.learners = giveMeTunedLearners(learners = learners, tune.controls = tune.controls, rsi = resampling.inner)
resamplings = replicate(n = length(tasks), resampling.outer, simplify = FALSE)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))

reg = makeExperimentRegistry(e.string, packages = c("mlr", "mlrMBO"))
batchmark(reg, learners = tuned.learners, tasks = tasks, resamplings, measures = list(mmce, timetrain), overwrite = TRUE, repls = 1, save.opt.result = TRUE)
#rres = testJob(reg, 1, external = FALSE)
submitJobs(reg, sample(getJobIds(reg)))
all.res = reduceResultsList(reg = reg)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))

#####
##### Visualisierung ####
#####

giveMeVisuals(all.res, e.string, init.design.points = tune.controls[[1]]$mbo.control$init.design.points)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))
