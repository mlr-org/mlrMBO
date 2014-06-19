makeMultiFidLearner = function(surrogat.learner, par.set, fid.param.id) {
  fid.param = par.set$pars[[fid.param.id]] #set + string = extract param
  fid.vals = fid.param$values
  lrn.list = replicate(length(fid.vals), surrogat.learner, simplify = FALSE)
  names(lrn.list) = fid.vals
  makeS3Obj(c("MultiFidLearner", "Learner"),
    learners = lrn.list,
    fid.param = fid.param
  )
}

# trains the learners inside of MultiFidLearner with on task data and
# returns ????  a list splits automaticaly ??
train.MultiFidLearner = function(obj, task) {
  # some shortcuts
  learner = obj
  fid.par = learner$fid.param
  fid.vals = as.numeric(fid.par$values)
  fid.ns = names(fid.par$values)
  data = getTaskData(task)
  cns = colnames(data)

  if (!all(learner$fid.param$values %in% data[[learner$fid.param$id]]))
    stopf("MultiFid model has to be initialized on all values of '%s'", learner$fid.param)
  models = namedList(fid.ns, NULL) #init named empty model list
  v.prev = NULL
  for (v in fid.vals) {
    r.inds = which(data[[fid.par$id]] == v)
    c.names = setdiff(cns, fid.par$id)
    sub.task = subsetTask(task, subset = r.inds, features = c.names)
    if (is.null(v.prev)) {
      #train base model for first perf.val
      models[[as.character(v)]] =
        train(learner = learner$learners[[as.character(v)]], task = sub.task)
    } else {
      #train deltas
      #calculcate surrogate values of one level below (usage of predict.MultiFid would be awsome here)
      preds = lapply(models[learner$fid.param$values < v], predict, task = sub.task)
      # FIXME: bad code, update BBmisc
      preds.response = extractSubList(extractSubList(preds, element = "data", simplify = FALSE), element = "response")
      traindata = getTaskData(sub.task)
      traindata$y = traindata$y - rowSums(preds.response)
      sub.task = mlr:::changeData(task = sub.task, data = traindata)
      models[[as.character(v)]] =
        train(learner = learner$learners[[as.character(v)]], task = sub.task)
    }
    v.prev = v
  }
  makeS3Obj(c("MultiFidModel","WrappedModel"), learner = learner, models = models)
}

# FIXME: updateMultiFidModel a better name?
train.MultiFidModel = function(obj, task) {
  # FIXME: implement updating mechanism here later!
  learner = obj$learner
  train.MultiFidLearner(learner, task)
}

predict.MultiFidModel = function(model, task, newdata, ...) {
  # some shortcuts
  fid.par = model$learner$fid.param
  fid.vals = as.numeric(fid.par$values)
  fid.ns = names(fid.par$values)

  # we calculate a lot of unneccessary things here (for now)
  #e ignore the given perf.val in (newdata, task) and calcuate it for all
  if (!missing(newdata)) {
    nd.fid.vals = newdata[[fid.par$id]]
    newdata = newdata[,  setdiff(colnames(newdata) , fid.par$id), drop = FALSE]
    preds = lapply(model$models, mlr:::predict.WrappedModel, newdata = newdata)
  } else {
    nd.fid.vals = getTaskData(task)[[fid.par$id]]
    task = dropFeatures(task, fid.par$id)
    preds = lapply(model$models, mlr:::predict.WrappedModel, task = task)
  }

  #calculate means + ses for each perf.val step
  #calculate means for each perf.val step
  getByVal = function(el) {
    res = lapply(fid.vals, function(v) {
      els = lapply(preds[fid.vals <= v], function(x) x$data[[el]])
      Reduce('+', els)
    })
    setNames(res, fid.ns)
  }
  means.by.val = getByVal("response")
  ses.by.val = getByVal("se")

  #FIXME: the following lines look suspicious. looks MUCH too complicated. what do we do here?
  #extract the values according to the perf.val value in the given (newdata,task) and place them in the correct order
  mean = sapply(seq_along(nd.fid.vals), function(i) {
    means.by.val[[as.character(nd.fid.vals[i])]][i]
  })
  se = sapply(seq_along(nd.fid.vals), function(i) {
    ses.by.val[[as.character(nd.fid.vals[i])]][i]
  })
  p = preds[[1L]]
  p$data$response = mean
  p$data$se = se
  p
}
