#' @title Fuse learner with imputation of y
#'
#' @description
#' This wrapper will train the underling learner with the set of the task that has no NAs in y.
#' For the missing y values we will predict a set of quantiles.
#' All combinations of those quantile values will be taken to replace the missing values leading to multiple different new tasks.
#' We then will train a model for each of those tasks.
#'
#' @param learner [\code{Learner}]\cr
#'   mlr Learner
#' @param aw.quantiles [\code{numeric}]\cr
#'   Quantiles that should be calculated for imputation.
#'   Default is \code{c(0.25,0.5, 0.75)}.
#' @return Wrapped Learner
#' @family wrapper
makeAsynWrapper = function(learner, aw.quantiles = c(0.25,0.5,0.75)) {
  learner = mlr:::checkLearner(learner, type=c("regr"))
  pv = list()
  assertNumeric(aw.quantiles, lower = 0, upper = 1, any.missing = FALSE, min.len = 1)
  pv$aw.quantiles = aw.quantiles
  id = paste(learner$id, "asyn", sep = ".")
  packs = learner$package
  ps = makeParamSet(
    makeNumericVectorLearnerParam(id = "aw.quantiles", lower = 0, upper = 1)
  )
  mlr:::makeHomogeneousEnsemble(id, learner$type, learner, packs, par.set = ps, par.vals = pv,
                          learner.subclass = "AsynWrapper", model.subclass = "AsynModel")
}

#' @export
trainLearner.AsynWrapper = function(.learner, .task, .subset, .weights = NULL, aw.quantiles = c(0.25,0.5,0.75), ...) {
  .task = subsetTask(.task, subset = .subset)
  learner = .learner$next.learner
  na.ind = is.na(getTaskTargets(.task))
  # train model on data where y is not missing
  model0 = train(learner = learner, task = subsetTask(.task, subset = !na.ind))
  if (all(!na.ind)) {
    models = list(model0)
  } else {
    p = predict(model0, subsetTask(.task, subset = na.ind))
    # calculate quantiles
    q.x = lapply(aw.quantiles, function(q) {
      qnorm(p = q, mean = getPredictionResponse(p), sd = getPredictionSE(p))
    })
    q.x = convertListOfRowsToDataFrame(q.x)
    # build all possible combinations of possible quantile outcomes
    # each column represents the possible outcomes for one missing Y value
    q.x.expanded = do.call(expand.grid, q.x)
    
    # now fill the missing Y values with all different combinations and train the models
    args = list(task = .task, q.x.expanded = q.x.expanded, learner = learner, na.ind = na.ind)
    parallelLibrary("mlr", master = FALSE, level = "mlrMBO.asynwrapper", show.info = FALSE)
    mlr:::exportMlrOptions(level = "mlrMBO.asynwrapper")
    models = parallelMap(doAsynWrapperTrainIteration, i = seq_len(nrow(q.x.expanded)), more.args = args, level = "mlrMBO.asynwrapper")
  }
  mlr:::makeHomChainModel(.learner, models)
}

doAsynWrapperTrainIteration = function(i, task, q.x.expanded, learner, na.ind) {
  data = getTaskData(task)
  data[na.ind, getTaskTargetNames(task)] = t(q.x.expanded[i, ])
  imputed.task = mlr:::changeData(task = task, data = data)
  train(learner = learner, task = imputed.task)
}

#' @export
predictLearner.AsynWrapper = function(.learner, .model, .newdata, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  parallelLibrary("mlr", master = FALSE, level = "mlrMBO.asynwrapper", show.info = FALSE)
  mlr:::exportMlrOptions(level = "mlrMBO.asynwrapper")
  ps = parallelMap(predict, models, more.args = list(newdata = .newdata))
  means = lapply(ps, getPredictionResponse)
  means = do.call(rbind, means)
  means = apply(means, 2L, mean)
  if (.learner$predict.type == "response") {
    return(means)  
  } else {
    ses = lapply(ps, getPredictionSE) 
    ses = do.call(rbind, ses)
    ses = apply(ses, 2L, mean)
    return(cbind(means, ses))
  }
}

#' @export
getLearnerProperties.AsynWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "missings")
}
