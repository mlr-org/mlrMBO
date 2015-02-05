#library(devtools)
#load_all("../mlr/")
#load_all()
library(mlrMBO)
library(mlr)
library(gridExtra)
library(foreign)
source("todo-files/multifid/benchmark/helpers.R")

set.seed(2)
e.lvls = c(0.1, 0.5, 1)
ctrl = makeMBOControl(
  init.design.points = length(e.lvls) * 4, 
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
  LiblineaRMultiClass = makeLearner("classif.LiblineaRMultiClass"),
  liblineaRBinary = makeLearner("classif.LiblineaRBinary", type = 1),
  ksvm = makeLearner("classif.ksvm"))
par.sets = list(
  LiblineaRMultiClass = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("epsilon", lower = -20, upper = 2, trafo = function(x) 2^x)),
  liblineaRBinary = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("epsilon", lower = -20, upper = 2, trafo = function(x) 2^x)),
  ksvm = makeParamSet(
    makeNumericParam("C", lower = -5, upper = 2, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -3, upper = 3, trafo = function(x) 2^x))
)

rsi = makeResampleDesc("Subsample", iters = 2)
# rsi = makeResampleDesc("Holdout")

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
  w7a = makeClassifTask(id = "w7a", data = libsvm.read("../data/w7a"), target = "Y"),
  #w8a = makeClassifTask(id = "w8a", data = libsvm.read("../data/w8a"), target = "Y"),
  #covtype = makeClassifTask(id = "covtype", data = read.arff("../data/covtype-normalized.arff"), target = "class")
  iris = iris.task
)

#tasks = c(tasks, list(
#  covtype.dummy = createDummyFeatures(tasks$covtype)
#  ))

#only one test now
lrn = lrns[[2]]
ps = par.sets[[2]]
task = tasks$w7a

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

#result df preparation
res.df = cbind.data.frame(
  convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("aggr"), simplify = FALSE)),
  convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("measures.test.sd"), simplify = FALSE)),
  convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("exec.times.mean"), simplify = FALSE)),
  convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("exec.times.sd"), simplify = FALSE)),
  convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("best.reached.at.mean"), simplify = FALSE), col.names = "best.reached.at.mean")
  )
res.df

num.col = sapply(res.df, is.numeric)
res.df[,num.col] = sapply(res.df[,num.col], function(x) sprintf("%.4g", x))

pdf(paste0("plots/CV_compare_table.pdf"), width = 14, height = 10)
grid.table(res.df)
dev.off()

save.image(file = "plots/CV_compare.RData")

tune.rres2$multiFid$ops.df
all.df = melt(extractSubList(tune.rres2, "ops.df", simplify = FALSE), measure.vars = c("mmce.test.mean.best", "mmce.test.mean.best.index", "exec.time.cum"))
all.df = rename(all.df, c("L1" = "learner.id"))
g = ggplot(data = all.df, aes(y = value, x = dob, color = learner.id))
g = g + geom_line(siez = 1, alpha = 0.5, mapping = aes(group = paste0(iter,learner.id)))
g = g + facet_wrap(~variable, scales = "free")
g = g + stat_summary(fun.y=mean, geom="line", size = 2, alpha = 0.9, mapping = aes(group = learner.id))
g = g + theme_bw()
g
