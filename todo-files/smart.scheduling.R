devtools::load_all("~/gits/mlr")
devtools::load_all()
#library(mlrMBO)
library(parallelMap)
#task einlesen
libsvm.read = function(file) {
  library("e1071")
  library("Matrix")
  dataset = read.matrix.csr(file)
  colNames = sapply( (1:(dim(dataset$x)[2])), FUN = function(x) { paste("X",x, sep = "") })
  dataframe = as.data.frame(as.matrix(dataset$x))
  colnames(dataframe) = colNames
  dataframe$Y = dataset$y
  dataframe
}
a9a = libsvm.read(file = "../data/a4a")
task = makeClassifTask(id = "a9a", data = a9a, target = "Y")

learner = makeModelMultiplexer(list(
  makeLearner("classif.randomForest", id = "classif.randomForest"),
  makeLearner("classif.svm", id = "classif.svm.radial", kernel = "radial"),
  makeLearner("classif.fnn", id = "classif.fnn") #as soon as I add this (even without the ParamSet) it generates the generateDesign warning -- seems right
))

ps.hp1 = makeModelMultiplexerParamSet(
  learner, 
  classif.svm.radial = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2^x)),
  classif.randomForest = makeParamSet(
    makeIntegerParam("mtry", lower = floor((getTaskNFeats(task)^1/4)), upper = ceiling((getTaskNFeats(task))^1/1.5)),
    makeIntegerParam("nodesize", lower = 1, upper = 10)),
  classif.fnn = makeParamSet(
    makeIntegerParam("k", lower = 1, upper = 10)
  )
)


mbo.ctrl = makeMBOControl(
  noisy = TRUE, 
  iters = 5, 
  init.design.points = 10,
  final.method = "best.predicted",
  propose.points = 20L,
  schedule.nodes = 3L,
  schedule.method = "smartParallelMap")

mbo.ctrl = setMBOControlInfill(
  mbo.ctrl, crit = "lcb",
  opt = "focussearch",
  opt.focussearch.maxit = 3,
  opt.focussearch.points = 1000)

mbo.ctrl.unsmart = mbo.ctrl
mbo.ctrl.unsmart$propose.points = mbo.ctrl.unsmart$schedule.nodes
mbo.ctrl.unsmart$schedule.method = "parallelMap"

surrogate.learner = makeLearner("regr.btgp", predict.type = "se")
surrogate.learner = makeImputeWrapper(surrogate.learner, classes = list(numeric = imputeConstant(10*2^15), factor = imputeConstant("NA"), integer = imputeConstant(10*2^15)))

mlr.ctrl = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl, mbo.keep.result = TRUE, continue = TRUE)
mlr.ctrl.unsmart = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl.unsmart, mbo.keep.result = TRUE, continue = TRUE)

inner.rdesc = makeResampleDesc("Holdout")
outer.rdesc = makeResampleDesc("CV", iter = 3)

lrn.tuned = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = mlr.ctrl, show.info = TRUE)
lrn.tuned.unsmart = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = mlr.ctrl.unsmart, show.info = TRUE)

#r1 = resample(learner = setHyperPars(learner, selected.learner = "classif.randomForest"), task = task, resampling = inner.rdesc)
#r2 = resample(learner = setHyperPars(learner, selected.learner = "classif.svm.radial"), task = task, resampling = inner.rdesc)

parallelStartMulticore(cpus = mbo.ctrl$schedule.nodes, level = "mlrMBO.feval")
#parallelStartBatchJobs(bj.resources = list(walltime = 60^2, ppn = 2, memory = 8000), level = "mlrMBO.feval")
#parallelStartMPI(level = "mlrMBO.feval", load.balancing = TRUE)
set.seed(1)
res = resample(lrn.tuned, task, outer.rdesc, measures = list(mmce, timetrain), extract = getTuneResult)
set.seed(1)
res.unsmart = resample(lrn.tuned.unsmart, task, outer.rdesc, measures = list(mmce, timetrain), extract = getTuneResult)
parallelStop()

### Bilder machen

trailingMin = function(x) {
  min.x = Inf
  for(i in seq_along(x)){
    if (x[i] < min.x) {
      min.x = x[i]
    } else {
      x[i] = min.x
    }
  }
  x
}

op.dfs = lapply(extractSubList(res$extract, c("mbo.result", "opt.path"), simplify = FALSE), as.data.frame)
op.dfs.unsmart = lapply(extractSubList(res.unsmart$extract, c("mbo.result", "opt.path"), simplify = FALSE), as.data.frame)

plot(trailingMin(as.data.frame(res$extract[[2]]$mbo.result$opt.path)$y), type = "l", ylim = c(0.14, 0.2), xlim = c(0, 30))
lines(trailingMin(as.data.frame(res.unsmart$extract[[2]]$mbo.result$opt.path)$y), col = "blue")
save.image("smart.scheduling.RData")


