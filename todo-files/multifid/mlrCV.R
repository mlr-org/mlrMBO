#library(devtools)
#load_all("../mlr/")
#load_all()
source("todo-files/multifid/benchmark/giveMe.R")

lrn = makeLearner("regr.svm")
task = bh.task
par.set = makeParamSet(
  makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -10, upper = 6, trafo = function(x) 2^x),
  makeNumericParam("epsilon", lower = -10, upper = 0, trafo = function(x) 2^x))

set.seed(1)

budget = 50L
measures = list(mse, timetrain)
mbo.control = makeMBOControl()
mlr.multiFid.control = mlr:::makeTuneControlMBO(
  mbo.control = mbo.control, 
  learner = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE))

lrn1 = makeDownsampleWrapper(lrn, dw.stratify = FALSE)
lrn2 = makeTuneWrapper(learner = lrn1, resampling = giveMeResampleDesc("inner"), measures = measures, par.set = par.set, control = mlr.multiFid.control, show.info = TRUE)

r = resample(lrn2, task, resampling = giveMeResampleDesc("outer"), measures, extract = getTuneResult)
