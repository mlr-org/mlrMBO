#library(devtools)
#load_all("../mlr/")
#load_all()
library(mlrMBO)
library(mlr)
library(gridExtra)
source("todo-files/multifid/benchmark/data_benchmark.R")

set.seed(2)
e.lvls = c(0.1, 0.25, 0.5, 1)
ctrl = makeMBOControl(
  init.design.points = length(e.lvls) * 5, 
  init.design.fun = maximinLHS,
  iters = 10L,
  on.learner.error = "stop",
  show.learner.output = FALSE
)
ctrl = setMBOControlInfill(
  control = ctrl, 
  opt = "focussearch", 
  opt.restarts = 1L, 
  opt.focussearch.maxit = 1L, 
  opt.focussearch.points = 100L,
  filter.proposed.points = TRUE,
  filter.proposed.points.tol = 0.01
)
ctrl.multiFid = setMBOControlMultiFid(
  control = ctrl, 
  param = "dw.perc", 
  lvls = e.lvls,
  cor.grid.points = 40L,
  costs = NULL #c(0.1, 0.2, 3)
)
ctrl.multiFid$infill.crit = "multiFid"

surrogat.learner = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)

lrns = list(
  liblineaR = makeLearner("classif.LiblineaRBinary", type = 1),
  ksvm = makeLearner("classif.ksvm"))
par.sets = list(
  liblineaR = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("epsilon", lower = -20, upper = 2, trafo = function(x) 2^x)),
  ksvm = makeParamSet(
    makeNumericParam("C", lower = -5, upper = 2, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -3, upper = 3, trafo = function(x) 2^x))
)

rsi = makeResampleDesc("CV", iters = 2)
#rsi = makeResampleDesc("Holdout")

mlr.mlrMBO.multiFid.control = mlr:::makeTuneControlMBO(mbo.control = ctrl.multiFid, learner = surrogat.learner)
mlr.mlrMBO.control = mlr:::makeTuneControlMBO(mbo.control = ctrl, learner = surrogat.learner)
iters = mlr.mlrMBO.control$mbo.control$init.design.points + mlr.mlrMBO.control$mbo.control$iters

mlr.tuneRandom.control = makeTuneControlRandom(maxit = iters)

tune.controls = list(
  multiFid = mlr.mlrMBO.multiFid.control,
  mlrMBO = mlr.mlrMBO.control,
  random = mlr.tuneRandom.control
)

tasks = list(
  sonar = sonar.task,
  w7a = makeClassifTask(id = "w7a", data = libsvm.read("../data/w7a"), target = "Y")
)

#only one test now
lrn = lrns[[1]]
ps = par.sets[[1]]
task = tasks[[2]]

tune.rres = lapply(names(tune.controls), function(tune.name) {
  lrn.dws = makeDownsampleWrapper(learner = lrn, dw.perc = 1, dw.stratify = TRUE)
  lrn.tuned = makeTuneWrapper(learner = lrn.dws, resampling = makeResampleDesc("Holdout"), measures = mmce, par.set = ps, control = tune.controls[[tune.name]])
  rres = resample(learner = lrn.tuned, task = task, resampling = rsi, extract = getTuneResult, measures = list(mmce, timetrain))  
  ops = extractSubList(rres$extract, "opt.path", simplify = FALSE)
  rres$exec.times = vnapply(ops, function(op) sum(getOptPathExecTimes(op)))
  rres$exec.times.mean = c(exec.times.mean = mean(rres$exec.times))
  rres$exec.times.sd = c(exec.times.sd = sd(rres$exec.times))
  rres$measures.test.sd = vnapply(rres$measures.test[,-1], sd)
  names(rres$measures.test.sd) = paste0(names(rres$measures.test.sd), ".sd")
  return(rres)
})
names(tune.rres) = names(tune.controls)

#result df preparation
res.df = cbind.data.frame(
  convertListOfRowsToDataFrame(extractSubList(tune.rres, c("aggr"), simplify = FALSE)),
  convertListOfRowsToDataFrame(extractSubList(tune.rres, c("measures.test.sd"), simplify = FALSE)),
  convertListOfRowsToDataFrame(extractSubList(tune.rres, c("exec.times.mean"), simplify = FALSE)),
  convertListOfRowsToDataFrame(extractSubList(tune.rres, c("exec.times.sd"), simplify = FALSE))
  )
res.df

num.col = sapply(res.df, is.numeric)
res.df[,num.col] = sapply(res.df[,num.col], function(x) sprintf("%.4g", x))

pdf(paste0("plots/CV_compare_table.pdf"), width = 14, height = 10)
grid.table(res.df)
dev.off()

save.image(file = "plots/CV_compare.RData")


