taks = iris.task
par.set = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2 ^ x),
    makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2 ^ x))
learner = makeLearner("classif.svm", kernel = "radial")
resampling = makeResampleDescription("CV")
k = 3

obj.fun = makeSingleObjectiveFunction(
  fn = function(x) {
    lrn2 = setHyperPars(lrn, removeMissingValues(x))
    res = resample(lrn2, task, resampling, measures = measures)
    res$aggr
  }, par.set = par.set, has.simple.signature = FALSE, noisy = TRUE)

mbo.ctrl = makeMBOControl(
  propose.points = 1,
  schedule.nodes = k,
  save.file.path = "~/mbo_asyn/",
  schedule.method = "asyn")

mbo.ctrl = setMBOControlInfill(mbo.ctrl,
  crit = "cb",
  crit.cb.lambda = 2)

