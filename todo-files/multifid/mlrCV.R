#library(devtools)
#load_all("../mlr/")
#load_all()
library(mlrMBO)
library(mlr)
library(ggplot2)
library(gridExtra)
library(foreign)
source("todo-files/multifid/benchmark/helpers.R")
source("todo-files/multifid/benchmark/giveMe.R")

set.seed(030)

surrogat.learner = giveMeSurrogatLearner()
rsi = giveMeResampleDesc("cv")
tune.controls = list(
  mlr.multiFid.control = mlr:::makeTuneControlMBO(mbo.control = giveMeMBOMultiFidControl(), learner = surrogat.learner),
  mlr.multiFid.highFidelity.control = mlr:::makeTuneControlMBO(mbo.control = giveMeMBOMultiFidControl(e.lvls = giveMeLvl("big.data")), learner = surrogat.learner),
  mlr.multiFid.fixedCosts.control = mlr:::makeTuneControlMBO(mbo.control = giveMeMBOMultiFidControl(e.lvls = giveMeLvl("std"), costs = giveMeLvl("std")^2), learner = surrogat.learner),
  mlr.mlrMBO.control = mlr:::makeTuneControlMBO(mbo.control = giveMeMBOControl(), learner = surrogat.learner)
)
tune.controls = c(tune.controls, list(
  mlr.tuneRandom.control = makeTuneControlRandom(maxit = giveMeResolution(tune.controls$mlr.multiFid.control)))
)

#only one test now
lrn = giveMeLearners("liblineaRBinary")[[1]]
ps = giveMeParamSets(list(lrn))[[1]]
task = giveMeTasks("w8a")[[1]]

tune.rres = lapply(names(tune.controls), function(tune.name) {
  lrn.dws = makeDownsampleWrapper(learner = lrn, dw.perc = 1, dw.stratify = TRUE)
  lrn.tuned = makeTuneWrapper(learner = lrn.dws, resampling = makeResampleDesc("Holdout"), measures = mmce, par.set = ps, control = tune.controls[[tune.name]])
  rres = resample(learner = lrn.tuned, task = task, resampling = rsi, extract = getTuneResult, measures = list(mmce, timetrain))  
  return(rres)
})
names(tune.rres) = names(tune.controls)
save.image(file = "plots/CV_compare.RData")

#now handle the data
tune.rres2 = lapply(tune.rres, function(rres) {
  ops = extractSubList(rres$extract, "opt.path", simplify = FALSE)
  rres$exec.times = vnapply(ops, function(op) sum(getOptPathExecTimes(op)))
  rres$exec.times.mean = c(exec.times.mean = mean(rres$exec.times))
  rres$exec.times.sd = c(exec.times.sd = sd(rres$exec.times))
  rres$measures.test.sd = vnapply(rres$measures.test[,-1], sd)
  names(rres$measures.test.sd) = paste0(names(rres$measures.test.sd), ".sd")
  rres$op.dfs = OpsAddByIter(ops, best.col = "mmce.test.mean")
  rres$best.reached.at = vnapply(rres$op.dfs, function(df) getLast(df$mmce.test.mean.best.index))
  rres$best.reached.at.mean = mean(rres$best.reached.at)
  rres$ops.df = do.call(rbind, rres$op.dfs)
  rres$extract = NULL
  return(rres)
})

save.image(file = "plots/CV_compare.RData")

pdf(paste0("plots/CV_compare_table.pdf"), width = 14, height = 10)
grid.table(giveMeResultTable(tune.rres2, pretty = TRUE))
dev.off()

save.image(file = "plots/CV_compare.RData")

all.df = melt(extractSubList(tune.rres2, "ops.df", simplify = FALSE), measure.vars = c("mmce.test.mean.best", "mmce.test.mean.best.index", "exec.time.cum"))
all.df = rename(all.df, c("L1" = "learner.id"))
g = giveMePlot(all.df)
ggsave(plot = g, filename = "plots/CV_compare_increase.pdf", width = 14, height= 6 )

g = giveMePlot(all.df[!(all.df$dob < 10),])
ggsave(plot = g, filename = "plots/CV_compare_increase_shortened.pdf", width = 14, height= 6 )


save.image(file = "plots/CV_compare.RData")