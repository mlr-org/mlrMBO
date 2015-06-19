makeTuningResult = function(stored.models = list(), resample.results = list(), mbo.result = NULL, final.index = NULL) {
  tuningResult = new.env()

  tuningResult$stored.models = stored.models
  tuningResult$resample.results = resample.results
  tuningResult$mbo.result = mbo.result
  tuningResult$final.index = final.index

  class(tuningResult) = append(class(tuningResult), "TuningResult")
  tuningResult
}

setTuningResultResampleResults = function(tuningResult, tuningState) {
    # small helper for learner resampling
  # if we have multiple tasks, return a list, otherwise singleton result

  loop = getTuningStateLoop(tuningState)
  tasks = getTuningStateTasks(tuningState)
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

  if (loop %in% control$resample.at)
    tuningResult$resample.results[[as.character(loop)]] = doResample(tasks)
  invisible()
}

setTuningResultStoredModels = function(tuningResult, tuningState) {
  loop = getTuningStateLoop(tuningState)
  control = getTuningProblemControl(tuningProblem)
  models = getTuningStateModels(tuningState)
  if (loop %in% control$store.model.at)
    tuningResult$stored.models[[as.character(loop)]] = if (length(models$models) == 1L) models$models[[1L]] else models$models
  invisible()
}