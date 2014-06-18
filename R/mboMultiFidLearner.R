makeMultiFidLearner = function(surrogat.learner, par.set, perf.param.id) {
  perf.param = par.set$pars[[perf.param.id]] #set + string = extract param
  perf.vals = perf.param$values
  lrn.list = replicate(length(perf.vals), surrogat.learner, simplify=FALSE)
  names(lrn.list) = perf.vals
  makeS3Obj(c("MultiFidLearner", "Learner"),learners = lrn.list, perf.param = perf.param)
}

train.MultiFidLearner = function(obj, task) {
  #trains the learners inside of MultiFidLearner with the date given in the task and returns a list
  #splits automaticaly
  learner = obj
  task.data = getTaskData(task)
  if (!all(learner$perf.param$values %in% task.data[[learner$perf.param$id]])) {
    stopf("MultiFid model has to be initialized on all values of %s", learner$perf.param)
  }
  models = sapply(learner$perf.param$values,function(x) NULL) #init named empty model list
  pref.val.prev = NULL
  for(perf.val in learner$perf.param$values) {
    r.inds = which(task.data[[learner$perf.param$id]] == perf.val)
    c.names = colnames(task.data)[colnames(task.data) %nin% learner$perf.param$id]
    sub.task = subsetTask(task, subset = r.inds, features = c.names)
    if(is.null(pref.val.prev)) {
      #train base model for first perf.val
      models[[as.character(perf.val)]] = 
        train(learner = learner$learners[[as.character(perf.val)]], task = sub.task)
    } else {
      #train deltas
      #calculcate surrogate values of one level below (usage of predict.MultiFid would be awsome here)
      preds = lapply(models[learner$perf.param$values < perf.val], predict, task = sub.task)
      preds.response = extractSubList(extractSubList(preds, element="data", simplify=FALSE), element="response")
      traindata = getTaskData(sub.task)
      traindata$y = traindata$y - rowSums(preds.response)
      sub.task = mlr:::changeData(task=sub.task, data=traindata)
      models[[as.character(perf.val)]] =
        train(learner = learner$learners[[as.character(perf.val)]], task = sub.task)
    } 
    pref.val.prev = perf.val
  }
  makeS3Obj(c("MultiFidModel","WrappedModel"), learner = learner, models = models)
}

train.MultiFidModel = function(obj, task) {
  # maybe Implement updating mechanism here later
  learner = obj$learner
  train.MultiFidLearner(learner, task)
}

predict.MultiFidModel = function(model, task, newdata, ...) {
  #we calculate a lot of unneccessary things here (for now)
  #we ignore the given perf.val in (newdata, task) and calcuate it for all
  if (!missing(newdata)) {
    perf.vals = newdata[[model$learner$perf.param$id]]
    newdata = newdata[,  setdiff(colnames(newdata) , model$learner$perf.param$id), drop = FALSE]
    preds = lapply(model$models, mlr:::predict.WrappedModel, newdata = newdata)
  } else {
    perf.vals = getTaskData(task)[[model$learner$perf.param$id]]
    features = colnames(getTaskData(task))
    task = subsetTask(task, features = setdiff(features, model$learner$perf.param$id))
    preds = lapply(model$models, mlr:::predict.WrappedModel, task = task)
  }
  if(length(perf.vals)==0){
    stop("No perf.vals found!")
  }
  #calculate means for each perf.val step
  means.by.val = lapply(model$learner$perf.param$values, function(perf.val) {
    means = lapply(preds[model$learner$perf.param$values <= perf.val], function(x) x$data$response)
    Reduce('+', means)
  })
  #calculate ses for each perf.val step
  ses.by.val = lapply(model$learner$perf.param$values, function(perf.val) {
    ses = lapply(preds[model$learner$perf.param$values <= perf.val], function(x) x$data$se)
    Reduce('+', ses)
  })
  #extract the values according to the perf.val value in the given (newdata,task) and place them in the correct order
  mean = sapply(seq_along(perf.vals), function(i) {
    means.by.val[[as.character(perf.vals[i])]][i]
  })
  se = sapply(seq_along(perf.vals), function(i) {
    ses.by.val[[as.character(perf.vals[i])]][i]
  })
  p = preds[[1L]]
  p$data$response = mean
  p$data$se = se
  p
}