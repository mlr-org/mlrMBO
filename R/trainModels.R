trainModels = function(learner, tasks, control) {

  models = vector("list", length(tasks))
  secs = 0

  for (i in seq_along(models)) {
    secs = secs + measureTime({
      models[[i]] = train(learner, tasks[[i]])
    })
  }

  list(models = models, train.time = secs)
}
