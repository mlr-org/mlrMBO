trainModel = function(learner, task, control) {
  st = system.time({
    m = train(learner, task)
  })
  return(list(model = m, train.time = st[3L]))
}

trainModels = function(learner, tasks, control) {
  z = lapply(tasks, trainModel,  learner = learner, control = control)
  list(
    models = extractSubList(z, "model", simplify = FALSE),
    train.time = sum(extractSubList(z, "train.time"))
  )
}

