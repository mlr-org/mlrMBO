library(devtools)
load_all("../mlr/")
load_all()
source("todo-files/multifid/benchmark/giveMe.R")

lrn = makeLearner("classif.LiblineaRMultiClass")
task = iris.task
par.set = giveMeParamSets(lrns = list(lrn))[[1]]

budget = 20L
measures = list(mmce, timetrain)
mlr.multiFid.control = makeTuneControlMBO(
  mbo.control = giveMeMBOMultiFidControl(budget = budget), 
  learner = giveMeSurrogatLearner())

lrn1 = makeDownsampleWrapper(lrn)
lrn2 = makeTuneWrapper(learner = lrn1, resampling = giveMeResampleDesc("inner"), measures = measures, par.set = par.set, control = mlr.multiFid.control, show.info = TRUE)

r = resample(lrn2, task, resampling = giveMeResampleDesc("outer"), measures, extract = getTuneResult)
