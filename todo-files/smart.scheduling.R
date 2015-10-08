library(mlrMBO)
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
a4a = libsvm.read(file = "~/gits/data/a4a")
task = makeClassifTask(id = "a4a", data = a4a, target = "Y")

ps.hp1 = makeParamSet(
  makeDiscreteParam("kernel", values = c("rbfdot", "polydot")),
  makeNumericParam("C", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -15, upper = 15, trafo = function(x) 2^x, requires = quote(kernel == "rbfdot")),
  makeIntegerParam("degree", lower = 1, upper = 5, requires = quote(kernel == "polydot")),
  makeNumericParam("scale", lower = 0, upper = 1, requires = quote(kernel == "polydot"))
)

learner = makeLearner("classif.ksvm")

mbo.ctrl = makeMBOControl(
  noisy = TRUE, 
  iters = 10, 
  init.design.points = 10,
  final.method = "best.predicted",
  propose.points = 9L,
  smart.schedule = 3L)

mbo.ctrl = setMBOControlInfill(
  mbo.ctrl, crit = "lcb",
  opt = "focussearch",
  opt.focussearch.maxit = 3,
  opt.focussearch.points = 1000)

mbo.ctrl.unsmart = mbo.ctrl
mbo.ctrl.unsmart$propose.points = 3L
mbo.ctrl.unsmart$smart.schedule = 1L

surrogate.learner = makeLearner("regr.randomForest", predict.type = "se")
surrogate.learner = makeImputeWrapper(surrogate.learner, classes = list(numeric = imputeConstant(10*2^15), factor = imputeConstant("NA"), integer = imputeConstant(10*2^15)))

mlr.ctrl = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl, mbo.keep.result = TRUE, continue = TRUE)
mlr.ctrl.unsmart = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl.unsmart, mbo.keep.result = TRUE, continue = TRUE)

inner.rdesc = makeResampleDesc("Holdout")
outer.rdesc = makeResampleDesc("CV", iter = 2)

lrn.tuned = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = mlr.ctrl, show.info = TRUE)
lrn.tuned.unsmart = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = mlr.ctrl.unsmart, show.info = TRUE)

parallelStartMulticore(cpus = 3L, level = "mlrMBO.feval")
res = resample(lrn.tuned, task, outer.rdesc, measures = list(mmce, timetrain), extract = getTuneResult)
res.unsmart = resample(lrn.tuned.unsmart, task, outer.rdesc, measures = list(mmce, timetrain), extract = getTuneResult)
parallelStop()

as.data.frame(res.unsmart$extract[[1]]$mbo.result$opt.path)
as.data.frame(res$extract[[1]]$mbo.result$opt.path)


