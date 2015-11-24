#devtools::load_all("~/gits/mlr")
#devtools::load_all()
library(mlrMBO)
library(parallelMap)
library(BatchExperiments)

# unlink("~/dump/ss", recursive = TRUE, force = TRUE)

#suggested: Number of phsy. CPUs
k = 4
#suggested 30
init.design.points = 30
#suggested 70
iters = 100
#suggested 10
cv.iters = 10
#walltime in hours 48 or Inf
walltime = 8


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

## Define Task
#a6a = libsvm.read(file = "../data/a6a")
w5a = libsvm.read(file = "../data/w5a")
tasks = list(
 # a6a = makeClassifTask(id = "a6a", data = a6a, target = "Y",
  w5a = makeClassifTask(id = "w5a", data = w5a, target = "Y")
)

# Wanna shortcut?
# k = 3
# init.design.points = 3
# iters = 3
# cv.iters = 2
# task = sonar.task

## Define Registry
reg = makeExperimentRegistry(
  id = "mbo_scheduling",
  file.dir = "~/nobackup/rambo",
  packages = c("mlr", "mlrMBO", "parallelMap"),
  multiple.result.files = FALSE,
  seed = 54119
)

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
    makeIntegerParam("mtry", lower = floor((getTaskNFeats(tasks[[1]])^1/4)), upper = ceiling((getTaskNFeats(tasks[[1]]))^1/1.5)),
    makeIntegerParam("nodesize", lower = 1, upper = 10),
    makeIntegerParam("ntree", lower = 50, upper = 1000)),
  classif.glmboost = makeParamSet(
    makeIntegerParam("mstop", lower = 50, upper = 1000)
  )
)

mbo.ctrl = makeMBOControl(
  noisy = TRUE, 
  init.design.points = init.design.points,
  final.method = "best.predicted",
  propose.points = k,
  schedule.nodes = k, 
  time.budget = walltime*60^2*0.97,
  save.on.disk.at.time = 60*30,
  save.file.path = file.path(reg$file.dir,"mboState.RData"))

mbo.ctrl = setMBOControlInfill(mbo.ctrl,
  opt = "focussearch",
  opt.focussearch.maxit = 5,
  opt.focussearch.points = 1000)

#rs, randomSearch
#r.lcb, randomLCB ohne scheduling
#r.s.lcb ~ mit scheduling
#r.s.time.lcb ~ mit Zeitsortierung
experiment.configurations = data.frame(
  propose.points = c(iters*k, k, rep(3*k, 2)),
  iters = c(1, rep(iters, 3)),
  infill.crit = c("random", rep("lcb", 3)),
  multipoint.method = c("random", rep("lcb", 3)),
  schedule.method = c(rep("none", 2), rep("smartParallelMap", 2)),
  multipoint.lcb.multiple = c(rep("random", 4)),
  schedule.priority.time = c(rep(FALSE, 3), TRUE),
  config.name = c("rs", "r.lcb", "r.s.lcb", "r.s.time.lcb"),
  stringsAsFactors = FALSE
)

surrogate.learner = makeLearner("regr.randomForest", predict.type = "se", nr.of.bootstrap.samples = 20, nodesize = ceiling(init.design.points/(2*length(ps.hp1$pars$selected.learner$values))), mtry = floor(getParamNr(ps.hp1)*6/7), ntree = 250)
surrogate.learner = makeImputeWrapper(surrogate.learner, classes = list(numeric = imputeConstant(10*2^15), factor = imputeConstant("NA"), integer = imputeConstant(10*2^15)))

mlr.ctrl = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl, mbo.keep.result = TRUE, continue = TRUE)

inner.rdesc = makeResampleDesc("Holdout")
outer.rdesc = makeResampleDesc("CV", iter = cv.iters)
measures = list(mmce, timetrain, timepredict)
lrn.tuned = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = mlr.ctrl, show.info = FALSE)

### BatchExp Stuff ####
algoMBOWrapper = function(lrn.tuned, measures) {
  force(lrn.tuned)
  force(measures)
  function(job, static, dynamic, ...) {
    mbo.pars = list(...)
    for (ctrl.name in names(mbo.pars)) {
      lrn.tuned$control$mbo.control[[ctrl.name]] = mbo.pars[[ctrl.name]]
    }
    lrn.tuned$control$mbo.control$init.design = dynamic$design
    lrn.tuned$control$mbo.control$save.file.path = gsub(".RData", sprintf("_%i.RData", job$id) , lrn.tuned$control$mbo.control$save.file.path)
    parallelStartMulticore(cpus = lrn.tuned$control$mbo.control$schedule.nodes, level = "mlrMBO.feval")
    set.seed(123 + dynamic$fold + job$repl)
    mod = train(learner = lrn.tuned, task = static$task, subset = dynamic$train)
    pred = predict(mod, task = static$task, subset = dynamic$test)
    parallelStop()
    perf = performance(pred, measures, task = static$task, model = mod)
    list(model = mod, performance = perf, fold = dynamic$fold, resources = getResources())
  }
}

dynamicResampling = function(lrn.tuned) {
  force(lrn.tuned)
  function(job, static, fold) {
    rin = makeResampleInstance(static$rdesc, task = static$task)
    design = generateDesign(lrn.tuned$control$mbo.control$init.design.points, par.set = lrn.tuned$opt.pars)
    list(train = rin$train.inds[[fold]], test = rin$test.inds[[fold]], fold = fold, design = design)
  }
}

## BatchExp Generate Stuff ####
pdes = lapply(tasks, function(task) {
  addProblem(reg = reg, id = task$task.desc$id,
             static = list(task = task, rdesc = outer.rdesc),
             dynamic = dynamicResampling(lrn.tuned),
             seed = reg$seed
  )
  makeDesign(id = task$task.desc$id, design = data.frame(fold = seq_len(outer.rdesc$iters)))
})

learners = list(lrn.tuned)
ades = lapply(learners, function(learner) {
  addAlgorithm(reg = reg, id = learner$id, fun = algoMBOWrapper(lrn.tuned = lrn.tuned, measures = measures))
  makeDesign(id = learner$id, design = experiment.configurations)
})

addExperiments(reg, prob.designs = pdes, algo.designs = ades, repls = 1, skip.defined = FALSE)

submitJobs(reg, ids = sample(findExperiments(reg)), resources = list(walltime = walltime*60^2, memory = 8000L, ppn = k*2))
waitForJobs(reg)

