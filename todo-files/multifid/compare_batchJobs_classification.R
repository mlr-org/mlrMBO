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
e.resources = list(walltime = 36*60^2)
e.resources.short = list(walltime = 8*60^2)

e.string = paste0("bJ_classification_",format(Sys.time(), "%Y_%m%d_%H%M"))
dir.create(paste0("../plots/", e.string), showWarnings = FALSE)

budget = 100000L
#budget = 30L
exec.time.budget = 4*60^2
time.budget = 24*60^2
tasks = giveMeTasks(c("meta_stream_intervals", "bng_cmc", "w7a", "w8a", "electricity", "covertype"))
#tasks = giveMeTasks("electricity")
resampling.inner = giveMeResampleDesc("inner")
resampling.outer = giveMeResampleDesc("cv")
#resampling.outer = giveMeResampleDesc("outer")
learners = giveMeLearners(c("svm"))
tune.controls = giveMeTuneControls(budget = budget, exec.time.budget = exec.time.budget, time.budget = time.budget)
tuned.learners = giveMeTunedLearners(learners = learners, tune.controls = tune.controls, rsi = resampling.inner)
resamplings = replicate(n = length(tasks), resampling.outer, simplify = FALSE)
names(resamplings) = names(tasks)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))

reg = makeExperimentRegistry(e.string, packages = c("mlr", "mlrMBO"))
batchmark(reg, learners = tuned.learners, tasks = tasks, resamplings, measures = list(mmce, timetrain), overwrite = TRUE, repls = 1, save.opt.result = TRUE)
#rres = testJob(reg, 1, external = FALSE)
df = getJobInfo(reg)
longs = substrRight(df$algo, 5) == "MBO_l"
jobs.short = df$id[!longs]
jobs.long = df$id[longs]
submitJobs(reg, sample(jobs.short), resources = e.resources.short, job.delay = TRUE)
Sys.sleep(60)
submitJobs(reg, sample(jobs.long), resources = e.resources, job.delay = TRUE)
waitForJobs(reg)
# df2 = getJobInfo(reg = reg, ids = findExpired(reg))
# jobs.more = setdiff(df2$id[substrRight(df2$algo, 2) %in% c("_l", "ow", "ts")], jobs.long)
# submitJobs(reg, sample(jobs.more), resources = e.resources)

all.res = reduceResultsList(reg = reg)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))

#####
##### Visualisierung ####
#####

giveMeVisuals(all.res, e.string, init.design.points = tune.controls[[1]]$mbo.control$init.design.points, tasks = tasks)

# save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))
