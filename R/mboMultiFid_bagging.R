makeMultiFidBaggingWrapper = function(learner) {
  learner = checkLearner(learner, type = "regr")
  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be 'response'.")
  x = mlr:::makeBaseWrapper(
    id = sprintf("%s.bagged", learner$id),
    next.learner = learner,
    package = learner$package,
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "iters", lower = 1L, default = 50L),
      makeNumericLearnerParam(id = "split", lower = 0, upper = 1, default = 2/3)),
    par.vals = list(),
    learner.subclass = "MultiFidBaggingWrapper",
    model.subclass = "BaggingModel"
  )
  addProperties(x, "se")
}


trainLearner.MultiFidBaggingWrapper = function(.learner, .task, .subset, .weights = NULL, iters = 50L, split = 2/3, ...) {
  .task = subsetTask(.task, subset = .subset)
  d = getTaskData(.task)
  allinds = seq_row(d)
  b = split(allinds, d$.multifid.lvl)
  models = replicate(iters, expr = {
    bags = lapply(b, function(x) {sample(x, size = ceiling(split * length(x)), replace = FALSE)})
    bag = sample(unlist(bags))
    w = .weights[bag]
    train(.learner$next.learner, .task, subset = bag, weights = w)
  }, simplify = FALSE)
  mlr:::makeChainModel(next.model = models, cl = "BaggingModel")
}


predictLearner.MultiFidBaggingWrapper = function(.learner, .model, .newdata, ...) {
  models = getBaggingModels(.model)
  p = asMatrixCols(lapply(models, function(m) {
    nd = .newdata[, m$features, drop = FALSE]
    predict(m, newdata = nd, ...)$data$response
  }))
  if (.learner$predict.type == "response") {
    rowMeans(p)
  } else {
    cbind(rowMeans(p), apply(p, 1L, sd))
  }
}


makeWrappedModel.MultiFidBaggingWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  addClasses(x, "BaggingModel")
}
