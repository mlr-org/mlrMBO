makeMultiFidLearner = function(surrogat.learner, par.set, control) {
  lrn.list = replicate(length(control$multifid.lvls), surrogat.learner, simplify = FALSE)
  names(lrn.list) = control$multifid.lvls
  makeS3Obj(c("MultiFidLearner", "Learner"),
    learners = lrn.list,
    fid.param = par.set$pars[[control$multifid.param]],
    fid.lvls = control$multifid.lvls
  )
}

# trains the learners inside of MultiFidLearner with on task data and
# returns ????  a list splits automaticaly ??
train.MultiFidLearner = function(obj, task) {
  # some shortcuts
  learner = obj
  fid.par = learner$fid.param
  fid.lvls = learner$fid.lvls
  fid.ns = as.character(fid.lvls)
  data = getTaskData(task)
  cns = colnames(data)

  if (!all(fid.lvls %in% data[[fid.par$id]]))
    stopf("MultiFid model has to be initialized on all values of '%s'", paste(fid.lvls, collapse=", "))
  models = namedList(fid.ns, NULL) #init named empty model list
  v.prev = NULL
  for (v in fid.lvls) {
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
      preds = lapply(models[fid.lvls < v], predict, task = sub.task)
      preds.response = extractSubList(xs = preds, element = c("data", "response"), simplify = TRUE)
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

update.MultiFidModel = function(obj, task) {
  # FIXME: implement updating mechanism here later!
  learner = obj$learner
  train.MultiFidLearner(learner, task)
}

predict.MultiFidModel = function(model, task, newdata, ...) {
  # some shortcuts
  fid.par = model$learner$fid.param
  fid.lvls = model$learner$fid.lvls
  fid.ns = as.character(fid.lvls)

  # we calculate a lot of unneccessary things here (for now)
  # we ignore the given perf.val in (newdata, task) and calcuate it for all
  if (!missing(newdata)) {
    nd.fid.lvls = newdata[[fid.par$id]]
    newdata = newdata[,  setdiff(colnames(newdata) , fid.par$id), drop = FALSE]
    preds = lapply(model$models, mlr:::predict.WrappedModel, newdata = newdata)
  } else {
    nd.fid.lvls = getTaskData(task)[[fid.par$id]]
    task = dropFeatures(task, fid.par$id)
    preds = lapply(model$models, mlr:::predict.WrappedModel, task = task)
  }

  #calculate means + ses for each perf.val step
  #calculate means for each perf.val step
  getByVal = function(el) {
    res = lapply(fid.lvls, function(v) {
      els = lapply(preds[fid.lvls <= v], function(x) x$data[[el]])
      Reduce('+', els)
    })
    setNames(res, fid.ns)
  }
  means.by.val = getByVal("response")
  ses.by.val = getByVal("se")

  #FIXME: the following lines look suspicious. looks MUCH too complicated. what do we do here?
  #extract the values according to the perf.val value in the given (newdata,task) and place them in the correct order
  mean = sapply(seq_along(nd.fid.lvls), function(i) {
    means.by.val[[as.character(nd.fid.lvls[i])]][i]
  })
  se = sapply(seq_along(nd.fid.lvls), function(i) {
    ses.by.val[[as.character(nd.fid.lvls[i])]][i]
  })
  p = preds[[1L]]
  p$data$response = mean
  p$data$se = se
  p
}
