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

e.string = paste0("bJ_",format(Sys.time(), "%Y_%m_%d-%H%M"))
dir.create(paste0("plots/", e.string), showWarnings = FALSE)

budget = 50L
reg = makeExperimentRegistry("mlr_benchmark", packages = c("mlr", "mlrMBO"))
tasks = giveMeTasks(c("sonar", "w7a", "w8a", "covtype.binary"))
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

save.image(file = paste0("plots/",e.string,"/CV_compare.RData"))

#####
##### Visualisierung ####
#####

task.ids = extractSubList(all.res, c("task.id"))
learner.ids = extractSubList(all.res, c("opt.result", "learner", "next.learner", "id"))
combs = unique(data.frame(task.ids, learner.ids))

visualize = function(task.id, learner.id) {
  res.by.task.learner = all.res[task.ids == task.id & learner.ids == learner.id]
  res.by.resampling = split(res.by.task.learner, extractSubList(res.by.task.learner, "learner.id"))
  this.e.string = paste0(e.string,"/", task.id, "_", learner.id)
                         
  tune.rres2 = lapply(res.by.resampling, function(rres) {
    res = list()
    ops = extractSubList(rres, c("opt.result", "opt.path"), simplify = FALSE)
    res$exec.times = vnapply(ops, function(op) sum(getOptPathExecTimes(op)))
    res$exec.times.mean = c(exec.times.mean = mean(res$exec.times))
    res$exec.times.sd = c(exec.times.sd = sd(res$exec.times))
    res$measures.test = convertListOfRowsToDataFrame(extractSubList(rres, "performance", simplify = FALSE))
    res$aggr = vnapply(res$measures.test, mean)
    names(res$aggr) = paste0(names(res$aggr),".test.mean")
    res$measures.test.sd = vnapply(res$measures.test, sd)
    names(res$measures.test.sd) = paste0(names(res$aggr),".test.sd")
    res$op.dfs = OpsAddByIter(ops, best.col = "mmce.test.mean")
    res$best.reached.at = vnapply(res$op.dfs, function(df) getLast(df$mmce.test.mean.best.index))
    res$best.reached.at.mean = mean(res$best.reached.at)
    res$ops.df = do.call(rbind, res$op.dfs)
    return(res)
  })
  
  pdf(paste0("plots/",this.e.string,"_CV_compare_table.pdf"), width = 16, height = 5)
  grid.table(giveMeResultTable(tune.rres2, pretty = TRUE))
  dev.off()
  
  all.df = melt(extractSubList(tune.rres2, "ops.df", simplify = FALSE), measure.vars = c("mmce.test.mean.best", "mmce.test.mean.best.index", "exec.time.cum"))
  all.df = rename(all.df, c("L1" = "learner.id"))
  g = giveMePlot(all.df)
  ggsave(plot = g, filename = paste0("plots/",this.e.string,"_CV_compare_increase.pdf"), width = 14, height= 6 )
  
  g = giveMePlot(all.df[!(all.df$dob < 10),])
  ggsave(plot = g, filename = paste0("plots/",this.e.string,"_CV_compare_increase_shortened.pdf"), width = 14, height = 6)
  
  invisible(NULL)
}

apply(combs, 1, function(x) visualize(x["task.ids"], x["learner.ids"]))

save.image(file = paste0("plots/",e.string,"/CV_compare.RData"))
