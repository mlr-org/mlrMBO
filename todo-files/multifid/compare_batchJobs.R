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

e.string = paste0("bJ_covertype_",format(Sys.time(), "%Y_%m_%d-%H%M"))
dir.create(paste0("../plots/", e.string), showWarnings = FALSE)

budget = 50L
reg = makeExperimentRegistry("mlr_benchmark", packages = c("mlr", "mlrMBO"))
tasks = giveMeTasks(c("iris", "covertype"))
resampling.inner = giveMeResampleDesc("inner")
resampling.outer = giveMeResampleDesc("cv")
learners = giveMeLearners(c("LiblineaRBinary", "svm"))
surrogat.learner = giveMeSurrogatLearner()
tune.controls = list(
  mlr.multiFid.control = mlr:::makeTuneControlMBO(
    mbo.control = giveMeMBOMultiFidControl(budget = budget), 
    learner = surrogat.learner),
  mlr.multiFid.fixedCosts.control = mlr:::makeTuneControlMBO(
    mbo.control = giveMeMBOMultiFidControl(
      e.lvls = giveMeLvl("std"), 
      costs = giveMeLvl("std")^2,
      budget = budget), 
    learner = surrogat.learner),
  mlr.multiFid.highFidelity.control = mlr:::makeTuneControlMBO(
    mbo.control = giveMeMBOMultiFidControl(
      e.lvls = giveMeLvl("big.data"),
      budget = budget), 
    learner = surrogat.learner),
  mlr.mlrMBO.control = mlr:::makeTuneControlMBO(
     mbo.control = giveMeMBOControl(budget = budget), 
     learner = surrogat.learner)
)
tune.controls = c(tune.controls, list(
  mlr.tuneRandom.control = makeTuneControlRandom(maxit = giveMeResolution(tune.controls$mlr.multiFid.control)))
)
tuned.learners = giveMeTunedLearners(learners = learners, tune.controls = tune.controls, rsi = resampling.inner)
resamplings = replicate(n = length(tasks), resampling.outer, simplify = FALSE)

#resample(learner = tuned.learners[[3]], task = tasks[[1]], resampling = resampling.inner, measures = mmce)

batchmark(reg, learners = tuned.learners, tasks = tasks, resamplings, measures = list(mmce, timetrain), overwrite = TRUE, repls = 1, save.opt.result = TRUE)
rres = testJob(reg, 1, external = FALSE)
submitJobs(reg, getJobIds(reg))
all.res = reduceResultsList(reg = reg)

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))

#####
##### Visualisierung ####
#####

giveMeVisuals(all.res, e.string, init.design.points = tune.controls[[1]]$mbo.control$init.design.points) {

save.image(file = paste0("../plots/",e.string,"/CV_compare.RData"))
