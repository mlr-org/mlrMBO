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
e.resources = list(walltime = 4*24*60^2)
# e.resources = list(walltime = 1*60^2)

e.string = paste0("bJ_YearPredictionMSD_",format(Sys.time(), "%Y_%m%d_%H%M"))
dir.create(paste0("../plots/", e.string), showWarnings = FALSE)

budget = 100000L
#budget = 30L
exec.time.budget = 3*24*60^2
time.budget = 3.5*24*60^2
data = read.table("../data/YearPredictionMSD.txt", header = FALSE, sep = ",")
colnames(data) = c("year", paste0("feat.",1:90))
tasks = makeRegrTask(id = "YearPredictionMSD", data = data, target = "year")
#tasks = giveMeTasks("electricity")
resampling.inner = giveMeResampleDesc("inner")
resampling.outer = makeResampleDesc("Subsample", iters = 2)
learners = giveMeLearners(c("regr.svm"))
surrogat.learner = giveMeSurrogatLearner()
tune.controls = list(
  mfMBO.low = mlr:::makeTuneControlMBO(
    mbo.control = giveMeMBOMultiFidControl(
      e.lvls = c(0.01, 0.02, 0.05, 0.1, 0.2, 1),
      budget = budget, exec.time.budget = exec.time.budget, time.budget = time.budget,
      init.design.points = 18L),
    learner = surrogat.learner),
  mlrMBO = mlr:::makeTuneControlMBO(
    mbo.control = giveMeMBOControl(
      budget = budget, exec.time.budget = exec.time.budget, time.budget = time.budget, 
      init.design.points = 4L), 
    learner = surrogat.learner,
    final.dw.perc = 1),
  RandomSearch = makeTuneControlRandom(maxit = budget, exec.time.budget = exec.time.budget, time.budget = time.budget, final.dw.perc = 1)
)
tuned.learners = giveMeTunedLearners(learners = learners, tune.controls = tune.controls, rsi = resampling.inner, measures = mse)
resamplings = list(resampling.outer)

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
