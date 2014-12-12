makeMultiFidBaggingWrapper = function(learner) {
  learner = checkLearner(learner, type = "regr")
  if (learner$predict.type != "response")
    stop("Predict type of the basic learner must be 'response'.")
  x = mlr:::makeHomogeneousEnsemble(
    id = sprintf("%s.bagged", learner$id),
    next.learner = learner,
    package = learner$package,
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "iters", lower = 1L, default = 50L),
      makeNumericLearnerParam(id = "split", lower = 0, upper = 1, default = 2/3)),
    par.vals = list(),
    learner.subclass = "MultiFidBaggingWrapper",
    model.subclass = "MultiFidBaggingModel"
  )
  addProperties(x, "se")
}


#' @export
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
  mlr:::makeHomChainModel(.learner, models)
}


predictLearner.MultiFidBaggingWrapper = function(.learner, .model, .newdata, ...) {
  p = mlr:::predictHomogeneousEnsemble(.learner, .model, .newdata, ...)
  # FIXME: the next line could be done in mlr::HomoEns
  if (.learner$predict.type == "response") {
    rowMeans(p)
  } else {
    cbind(rowMeans(p), apply(p, 1L, sd))
  }
}


