trainModels = function(learner, tasks, control) {
  if (control$multifid)
    learner = makeMultiFidWrapper(learner, control)

  n = length(tasks)
  models = vector("list", n)
  train.time = double(n)

  for (i in seq_len(n)) {
    st = system.time({
      models[[i]] = train(learner, tasks[[i]])
    })
    train.time[i] = st[3L]
  }

  list(models = models, train.time = train.time)
}
