#devtools::load_all("~/gits/mlr")
#devtools::load_all()
library(mlrMBO)
library(parallelMap)
library(BatchExperiments)

reg = makeExperimentRegistry(
  id = "mbo_scheduling",
  file.dir = "~/mbo_scheduling/",
  packages = c("mlr", "mlrMBO", "parallelMap"),
  multiple.result.files = FALSE,
  seed = 54119
)

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
a9a = libsvm.read(file = "../data/a9a")
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
  iters = 50, 
  init.design.points = 30,
  final.method = "best.predicted",
  propose.points = 30L,
  schedule.nodes = 7L,
  schedule.method = "smartParallelMap")

mbo.ctrl = setMBOControlInfill(
  mbo.ctrl, crit = "lcb",
  opt = "focussearch",
  opt.focussearch.maxit = 3,
  opt.focussearch.points = 1000)

schedule.methods = c("none", rep("smartParallelMap", times = 4))
schedule.priorities = c("infill", "infill", "explore", "exploit", "balanced")

surrogate.learner = makeLearner("regr.randomForest", predict.type = "se")
surrogate.learner = makeImputeWrapper(surrogate.learner, classes = list(numeric = imputeConstant(10*2^15), factor = imputeConstant("NA"), integer = imputeConstant(10*2^15)))

mlr.ctrl = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl, mbo.keep.result = TRUE, continue = TRUE)

doExperiment = function(schedule.method, schedule.priority) {
  this.mlr.ctrl = mlr.ctrl
  this.mlr.ctrl$mbo.control$schedule.method = schedule.method
  this.mlr.ctrl$mbo.control$schedule.priority= schedule.priority
  
  inner.rdesc = makeResampleDesc("Holdout")
  outer.rdesc = makeResampleDesc("CV", iter = 3)
  
  lrn.tuned = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = this.mlr.ctrl, show.info = FALSE)
  parallelStartMulticore(cpus = mbo.ctrl$schedule.nodes, level = "mlrMBO.feval")
  set.seed(1)
  res = resample(lrn.tuned, task, outer.rdesc, measures = list(mmce, timetrain), extract = getTuneResult)
  parallelStop()
  return(res)
}

batchMap(reg, fun = doExperiment, schedule.method = schedule.methods, schedule.priority = schedule.priorities)

submitJobs(reg, resources = list(walltime = 48*60^2, memory = 8000L, queue = "long_quad", ppn = 8))
waitForJobs(reg)


stop("finished until here")
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


