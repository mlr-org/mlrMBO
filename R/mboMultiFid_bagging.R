makeMultiFidBaggingWrapper = function(learner) {
  learner = checkLearner(learner, type = "regr")
  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be 'response'.")
  addProperties(mlr:::makeBaseWrapper(
    id = sprintf("%s.bagged", learner$id),
    next.learner = learner,
    package = learner$package,
    par.set = makeParamSet(),
    par.vals = list(),
    cl = "MultiFidBaggingWrapper"), "se")
}


trainLearner.MultiFidBaggingWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  .task = subsetTask(.task, subset = .subset)
  blocking = .task$blocking
  if (!length(blocking))
    stop("MultiFidBaggingWrapper need blocking!")
  allinds = levels(blocking)
  models = lapply(seq_along(allinds), function(i) {
    bag.blocks = allinds[-i]
    bag = which(blocking %in% bag.blocks)
    w = .weights[bag]
    train(.learner$next.learner, .task, subset = bag, weights = w)
  })
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
