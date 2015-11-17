#devtools::load_all("~/gits/mlr")
#devtools::load_all()
library(mlrMBO)
library(parallelMap)
library(BatchExperiments)

reg = makeRegistry(
  id = "mbo_scheduling",
  file.dir = "/home/kotthaus/MBO/MemTraceR/mlrMBO/todo-files/mbo_scheduling/",
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
w7a = libsvm.read(file = "../data/w7a")
task = makeClassifTask(id = "w7a", data = w7a, target = "Y")

learner = makeModelMultiplexer(list(
  makeLearner("classif.randomForest", id = "classif.randomForest"),
  makeLearner("classif.svm", id = "classif.svm.radial", kernel = "radial"),
  makeLearner("classif.glmboost", id = "classif.glmboost")
))

ps.hp1 = makeModelMultiplexerParamSet(
  learner, 
  classif.svm.radial = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2^x)),
  classif.randomForest = makeParamSet(
    makeIntegerParam("mtry", lower = floor((getTaskNFeats(task)^1/4)), upper = ceiling((getTaskNFeats(task))^1/1.5)),
    makeIntegerParam("nodesize", lower = 1, upper = 10)),
  classif.mboost = makeParamSet(
    makeIntegerParam("mstop", lower = 10, upper = 500)
  )
)

k = 8 #suggested: Number of phsy. CPUs
init.design.points = 30
iters = 70

mbo.ctrl = makeMBOControl(
  noisy = TRUE, 
  init.design.points = init.design.points,
  final.method = "best.predicted",
  propose.points = k,
  schedule.nodes = k)

mbo.ctrl = setMBOControlInfill(mbo.ctrl,
  opt = "focussearch",
  opt.focussearch.maxit = 3,
  opt.focussearch.points = 1000)

#1 randomsearch
#2,3 mbo k
#4,5,6,7,8 mbo scheduled
experiment.configurations = data.frame(
  propose.points = c(iters*k,k,k,rep(3*k, 5)),
  iters = c(1, rep(iters, 7)),
  infill.crit = c("random", rep("lcb", 7)),
  schedule.methods = c(rep("none",3), rep("smartParallelMap", times = 5)),
  schedule.priorities = c(rep("infill", 5), "explore", "exploit", "balanced"),
  infill.crit.lcb.multiple = c(rep("random",2), "static.quantiles", "random", rep("static.quantiles", times = 4))
)
experiment.configurations

surrogate.learner = makeLearner("regr.randomForest", predict.type = "se", nr.of.bootstrap.samples = 20, nodesize = 2)
surrogate.learner = makeImputeWrapper(surrogate.learner, classes = list(numeric = imputeConstant(10*2^15), factor = imputeConstant("NA"), integer = imputeConstant(10*2^15)))

mlr.ctrl = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl, mbo.keep.result = TRUE, continue = TRUE)

### BatchExp Stuff ####
algoMBOWrapper = function(lrn, measures) {
  force(lrn)
  force(measures)
  function(job, static, dynamic, ...) {
    mbo.pars = list(...)
    for (ctrl.name in names(mbo.pars)) {
      lrn$control$mbo.control[[ctrl.name]] = mbo.pars[[ctrl.name]]
    }
    mod = train(learner = lrn, task = static$task, subset = dynamic$train)
    pred = predict(mod, task = static$task, subset = dynamic$test)
    perf = performance(pred, measures, task = static$task, model = mod)
    list(model = mod, performance = perf, fold = dynamic$fold, resources = getResources())
  }
}

dynamicResampling = function() {
  function(job, static, fold) {
    rin = makeResampleInstance(static$rdesc, task = static$task)
    list(train = rin$train.inds[[fold]], test = rin$test.inds[[fold]], fold = fold)  
  }
}

## BatchExp Generate Stuff ####
tasks = list(task)
pdes = lapply(tasks, function(task) {
  addProblem(reg = reg, id = task$task.desc$id,
             static = list(task = task, rdesc = rdesc),
             dynamic = dynamicResampling,
             seed = reg$seed
  )
  makeDesign(id = task$task.desc$id, design = data.frame(fold = seq_len(rdesc$iters)))
})

ades = lapply(learners, function(learner) {
  addAlgorithm(reg = reg, id = learner$id, fun = algoMBOWrapper(lrn = learner, measures = measures))
  makeDesign(id = learner$id)
})

addExperiments(reg, prob.designs = pdes, algo.designs = ades, repls = 1, skip.defined = FALSE)
doExperiment = function(schedule.method, schedule.priority, infill.crit.lcb.multiple, mlr.ctrl, learner, ps.hp1, mbo.ctrl, task) {
  this.mlr.ctrl = mlr.ctrl
  if (schedule.method == "none") {
    this.mlr.ctrl$mbo.control$propose.points = this.mlr.ctrl$mbo.control$schedule.nodes
  }
  this.mlr.ctrl$mbo.control$schedule.method = schedule.method
  this.mlr.ctrl$mbo.control$schedule.priority = schedule.priority
  this.mlr.ctrl$mbo.control$infill.crit.lcb.multiple = infill.crit.lcb.multiple
  
  inner.rdesc = makeResampleDesc("Holdout")
  outer.rdesc = makeResampleDesc("CV", iter = 5)
  
  lrn.tuned = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = this.mlr.ctrl, show.info = FALSE)
  parallelStartMulticore(cpus = mbo.ctrl$schedule.nodes, level = "mlrMBO.feval")
  set.seed(1)
  res = resample(lrn.tuned, task, outer.rdesc, measures = list(mmce, timetrain), extract = getTuneResult)
  parallelStop()
  return(res)
}

batchMap(reg, fun = doExperiment, schedule.method = schedule.methods, schedule.priority = schedule.priorities, more.args = list(mlr.ctrl = mlr.ctrl, learner = learner, ps.hp1 = ps.hp1, mbo.ctrl = mbo.ctrl, task = task))

submitJobs(reg, resources = list(walltime = 48*60^2, memory = 8000L, queue = "long_quad", ppn = 8))
waitForJobs(reg)
