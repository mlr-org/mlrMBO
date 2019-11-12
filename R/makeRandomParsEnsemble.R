#' @title Build ensemble of multiple learners with sampled hyperparameters.
#'
#' @description
#' Define a learner and define which hyperparameters should get sampled.
#' The ensemble will be build of multiple learners, each with different random hyperparamters.
#' The predictions for mean and se will be averaged.
#'
#' @template arg_learner
#' @param samplers (`list`)\cr
#'   A named list of functions that create the random samples:
#'   e.g.: `list(coef.cov = function(n) rchisq(n = n, df = 3, ncp = 2))`
#' @template ret_learner
#' @family wrapper
#' @export
makeRandomParsEnsemble = function(learner, samplers, n.samples = 10L) {
  learner = checkLearner(learner, type = c("regr"))
  assertList(samplers, names = "named")

  id = stri_paste(learner$id, "RandomParsEnsemble", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeUntypedLearnerParam(id = "samplers"),
    makeIntegerLearnerParam(id = "n.samples", lower = 1, default = 10L)
  )
  pv = list(
    samplers = samplers,
    n.samples = n.samples
  )
  mlr:::makeHomogeneousEnsemble(id, learner$type, learner, packs, par.set = ps, par.vals = pv,
                          learner.subclass = "RandomParsEnsemble", model.subclass = "RandomParsEnsembleModel")
}

#' @export
print.RandomParsEnsembleModel = function(x, ...) {
  s = capture.output(print.WrappedModel(x))
  u = sprintf("Random Parameter Ensembles: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}

#' @export
trainLearner.RandomParsEnsemble = function(.learner, .task, .subset = NULL, .weights = NULL, samplers, n.samples = 10L, ...) {
  .task = subsetTask(.task, subset = .subset)

  args = list(task = .task, weights = .weights)

  sampled.pvs = Map(function(f) f(n.samples), samplers)
  sampled.pvs = do.call(Map, c(list(f = list), sampled.pvs)) # each list item one setting
  learners = Map(function(x) setHyperPars(.learner$next.learner, par.vals = x), sampled.pvs)

  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  mlr:::exportMlrOptions(level = "mlr.ensemble")
  models = parallelMap(doEnsembleTrainIteration, learner = learners, more.args = args, level = "mlr.ensemble")
  mlr:::makeHomChainModel(.learner, models)
}

doEnsembleTrainIteration = function(learner, task, weights) {
  mlr:::setSlaveOptions()
  train(learner, task, weights = weights)
}

#' @export
predictLearner.RandomParsEnsemble = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  preds = lapply(models, function(m) {
    predict(m, newdata = .newdata, subset = .subset, ...)$data[,c("response", "se")]
  })
  p = BBmisc::asMatrixCols(lapply(preds, function(x) x$response))
  se = BBmisc::asMatrixCols(lapply(preds, function(x) x$se))
  cbind(rowMeans(p), rowMeans(se))
}

# we need to override here. while the predtype of the encapsulated learner must always
# be response, we can estimates probs and se on the outside
#' @export
setPredictType.RandomParsEnsemble = function(learner, predict.type) {
  setPredictType.Learner(learner, predict.type)
}

#' @export
getLearnerProperties.RandomParsEnsemble = function(learner) {
  switch(learner$type,
         "classif" = union(getLearnerProperties(learner$next.learner), "prob"),
         "regr" = union(getLearnerProperties(learner$next.learner), "se")
  )
}

if (FALSE) {
  library("checkmate")
  library("stringi")
  library("mlrMBO")
  library("parallelMap")
  library("ggplot2")

  # debug example
  samplers = list(coef.cov = function(x) replicate(x, rchisq(2, df = 3, ncp = 2), simplify = FALSE), coef.var = function(x) rnorm(x)^2)
  n.samples = 3L
  lrn = makeLearner("regr.km", predict.type = "se")
  lrn2 = makeRandomParsEnsemble(lrn, samplers, 3L)

  fun = makeBraninFunction()
  des = generateDesign(5, par.set = getParamSet(fun))
  des$y = apply(des, 1, fun)
  tsk = makeRegrTask(data = des, target = "y")
  mod = train(lrn2, task = tsk)
  pred = predict(mod, tsk)

  # mbo example
  set.seed(1)

  fn = makeCosineMixtureFunction(1)
  obj.fun = convertToMinimization(fn)

  samplers = list(coef.cov = function(x) replicate(x, rchisq(smoof::getNumberOfParameters(obj.fun), df = 1, ncp = 2), simplify = FALSE), coef.var = function(x) rchisq(x, 1))
  n.samples = 3L
  lrn = makeLearner("regr.km", predict.type = "se")
  lrn2 = makeRandomParsEnsemble(lrn, samplers, 30L)

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 10L)
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), opt = "focussearch", opt.focussearch.points = 500L, opt.restarts = 1L)

  design = generateDesign(5L, getParamSet(obj.fun), fun = lhs::maximinLHS)

  run = exampleRun(obj.fun, design = design, learner = lrn2,
                   control = ctrl, points.per.dim = 1000, show.info = TRUE)

  for(i in 1:10) {
    plotExampleRun(run, iters = i, pause = FALSE, densregion = TRUE, gg.objects = list(theme_bw()))
  }

}
