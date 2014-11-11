
makeMultiFidBaggingWrapper = function(learner) {
  learner = checkLearner(learner, type="regr")
  pv = list()
  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be 'response'.")
  id = paste(learner$id, "bagged", sep = ".")
  packs = learner$package
  ps = makeParamSet()
  x = mlr:::makeBaseWrapper(id, learner, packs, par.set = ps, par.vals = pv, cl = ("MultiFidBaggingWrapper"))
  x = mlr::addProperties(x, "se")
  return(x)
}


trainLearner.MultiFidBaggingWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  .task = subsetTask(.task, subset = .subset)
  n = .task$task.desc$size
  blocking = .task$blocking
  if (!length(blocking)) {stop("MultiFidBaggingWrapper need blocking!")}
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


# we need to override here. while the predtype of the encapsulated learner must always
# be response, we can estimates probs and se on the outside

setPredictType.MultiFidBaggingWrapper = function(learner, predict.type) {
  learner = mlr:::setPredictType.Learner(learner, predict.type)
  return(learner)
}
