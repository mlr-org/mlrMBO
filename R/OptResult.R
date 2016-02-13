#' @title OptResult object.
#' @description
#' The OptResult stores all entities which are not needed while optimizing but are needed to build the final result.
#' It can contains fitted surrogate models at certain times as well as resample objects.
#' When the optimization ended it will contain the [\code{MBOResult}].
#' @name OptResult
#' @rdname OptResult
NULL


# @param stored.models [\code{list()}]\cr
#    A list of all models which we wanted to store during the tuning process.
#    Be aware that the true model is in \code{getOptResultStoredModels[[i]]$model}.
#    See \code{\link{trainModel}} for further details.
# @param resample.results [\code{list()}]\cr
#    Stores the resample result by mlr.
# @param mbo.result [\code{MBOResult}]\cr.
#    Will store one MBOResult. Usually the one created in \code{mbo()}, when all iterations are done.

# IMPORTANT NOTE:
# See this as a constructor and it's variables as member variables.
# All variables in this Object should be documented here.
# Think of it, when you implement new ones!
# Unfortunately in R we cannot hinder you from putting other values in this object, but please: Don't!
makeOptResult = function(stored.models = list(), resample.results = list(), mbo.result = NULL) {
  opt.result = new.env()

  opt.result$stored.models = stored.models
  opt.result$resample.results = resample.results
  opt.result$mbo.result = mbo.result

  class(opt.result) = append(class(opt.result), "OptResult")
  opt.result
}

# If the MBOControl says we should resample, we resample and store the result here
setOptResultResampleResults = function(opt.result, opt.state) {
  loop = getOptStateLoop(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  learner = getOptProblemLearner(opt.problem)
  control = getOptProblemControl(opt.problem)

  doResample = function(tasks) {
    if (length(tasks) == 1L)
      resample(learner, tasks[[1L]], control$resample.desc,
        measures = control$resample.measures, show.info = FALSE)
    else
      lapply(tasks, resample, learner = learner, resampling = control$resample.desc,
        measures = control$resample.measures, show.info = FALSE)
  }

  if (loop %in% control$resample.at) {
    tasks = getOptStateTasks(opt.state)
    opt.result$resample.results[[as.character(loop)]] = doResample(tasks)
  }
  invisible()
}

# If the MBOControl says we should store the model, store it here
setOptResultStoredModels = function(opt.result, opt.state) {
  loop = getOptStateLoop(opt.state)
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  if (loop %in% control$store.model.at) {
    models = getOptStateModels(opt.state)
    opt.result$stored.models[[as.character(loop)]] = if (length(models$models) == 1L) models$models[[1L]] else models$models
  }
  invisible()
}

getOptResultResampleResults = function(opt.result) {
  opt.result$resample.results
}

getOptResultStoredModels = function(opt.result) {
  opt.result$stored.models
}

setOptResultMboResult = function(opt.result, mbo.result) {
  opt.result$mbo.result = mbo.result
  invisible()
}

getOptResultMboResult = function(opt.result) {
  opt.result$mbo.result
}
