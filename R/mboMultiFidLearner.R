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
train.MultiFidLearner = function(obj, task, subset) {
  # some shortcuts
  learner = obj
  fid.par = learner$fid.param
  fid.lvls = learner$fid.lvls
  fid.ns = as.character(fid.lvls)
  data = getTaskData(task)
  cns = colnames(data)

  if (!all(fid.lvls %in% data[[fid.par$id]]))
    stopf("MultiFid model has to be initialized on all values of '%s'", collapse(fid.lvls))
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
      #FIXME: in case we have nearly a perfect constant shift between levels, kriging of course crashes here....
      models[[as.character(v)]] =
        train(learner = learner$learners[[as.character(v)]], task = sub.task)
    }
    v.prev = v
  }
  #FIXME: maybe use makeWrappedModel
  makeS3Obj(c("MultiFidModel","WrappedModel"), learner = learner, models = models, task.desc = task$task.desc)
}

update.MultiFidModel = function(obj, task) {
  # FIXME: implement updating mechanism here later!
  learner = obj$learner
  train.MultiFidLearner(learner, task)
}

predict.MultiFidModel = function(model, task, newdata, subset = NULL, ...) {
  # some shortcuts
  fid.par = model$learner$fid.param
  fid.lvls = model$learner$fid.lvls
  fid.ns = as.character(fid.lvls)

  # we calculate a lot of unneccessary things here (for now)
  # we ignore the given perf.val in (newdata, task) and calcuate it for all
  if (missing(newdata))
    newdata = getTaskData(task, subset = subset)
  else if (!is.null(subset)) {
    newdata = newdata[subset, , drop = FALSE]
  }
  fid.par.col = newdata[[fid.par$id]]
  newdata = newdata[,  setdiff(colnames(newdata) , fid.par$id), drop = FALSE] #remove column with fid.par
  split.inds = split(seq_row(newdata), fid.par.col)

  preds = lapply(fid.lvls, function(v) {
    this.data = subset(newdata, fid.par.col == v)
    preds.each = lapply(model$models[fid.lvls <= v], mlr:::predict.WrappedModel, newdata = this.data)
    se = extractSubList(preds.each, c("data", "se"), simplify = FALSE)
    response = extractSubList(preds.each, c("data", "response"), simplify = FALSE)
    pred = preds.each[[1]]
    pred$data$se = Reduce('+', se)
    pred$data$response = Reduce('+', response)
    pred
  })
  names(preds) = fid.lvls

  #initialize empty df for pred data in right order
  pred.data = do.call(rbind, extractSubList(preds, element = "data", simplify = FALSE)) #FIXME: Improve: Init an empty DF
  for (vs in fid.ns) {
    if (!is.null(split.inds[[vs]]))
      pred.data[split.inds[[vs]], ] = preds[[vs]]$data
  }
  #FIXME: maybe use makePrediction
  pred = preds[[1]]
  pred$data = pred.data
  pred$task.desc$size = sum(extractSubList(preds, c("task.desc","size")))

  pred
}
