# The Tuning Result keeps all the things we want to later give back to the user
# It is an enviroment and it is always pointed at by the TuningState.
# @param stored.models [\code{list()}]\cr
#    A list of all models which we wanted to store during the tuning process. Be aware that the true model is in \code{getTuningResultStoredModels[[i]]$model}. See \code{\link{trainModel}} for further details.
# @param resample.results [\code{list()}]\cr
#    Stores the resample result by mlr.
# @param mbo.result [\code{MBOResult}]\cr.
#    Will store one MBOResult. Usually the one created in \code{mbo()}, when all iterations are done.
makeTuningResult = function(stored.models = list(), resample.results = list(), mbo.result = NULL) {
  tuningResult = new.env()

  tuningResult$stored.models = stored.models
  tuningResult$resample.results = resample.results
  tuningResult$mbo.result = mbo.result

  class(tuningResult) = append(class(tuningResult), "TuningResult")
  tuningResult
}

# If the MBOControl says we should resample, we resample and store the result here
setTuningResultResampleResults = function(tuningResult, tuningState) {
  loop = getTuningStateLoop(tuningState)
  tuningProblem = getTuningStateTuningProblem(tuningState)
  learner = getTuningProblemLearner(tuningProblem)
  control = getTuningProblemControl(tuningProblem)

  doResample = function(tasks) {
    if (length(tasks) == 1L)
      resample(learner, tasks[[1L]], control$resample.desc, measures = control$resample.measures, show.info = FALSE)
    else
      lapply(tasks, resample, learner = learner, resampling = control$resample.desc,
        measures = control$resample.measures, show.info = FALSE)
  }

  if (loop %in% control$resample.at) {
    tasks = getTuningStateTasks(tuningState)
    tuningResult$resample.results[[as.character(loop)]] = doResample(tasks)
  }
  invisible()
}

# If the MBOControl says we should store the model, store it here
setTuningResultStoredModels = function(tuningResult, tuningState) {
  loop = getTuningStateLoop(tuningState)
  control = getTuningProblemControl(getTuningStateTuningProblem(tuningState))
  if (loop %in% control$store.model.at) {
    models = getTuningStateModels(tuningState)  
    tuningResult$stored.models[[as.character(loop)]] = if (length(models$models) == 1L) models$models[[1L]] else models$models
  }
  invisible()
}

getTuningResultResampleResults = function(tuningResult) {
  tuningResult$resample.results
}

getTuningResultStoredModels = function(tuningResult) {
  tuningResult$stored.models
}

setTuningResultMboResult = function(tuningResult, mbo.result) {
  tuningResult$mbo.result = mbo.result
  invisible()
}

getTuningResultMboResult = function(tuningResult) {
  tuningResult$mbo.result
}
