makeMultiFidLearner = function(learner, par.set, control) {
# 
#   makeS3Obj(c("MultiFidLearner", "Learner"),
#     learners = lrn.list,
#     fid.param = par.set$pars[[control$multifid.param]],
#     fid.lvls = control$multifid.lvls
#   )
  multifid.par.set = makeParamSet(makeUntypedLearnerParam(id = "mbo.control"))
  par.set = c(par.set, multifid.par.set)
  par.vals = list(mbo.control = control)
  id = paste(learner$id, "multifid", sep = ".")
  mlr:::makeBaseWrapper(id = id, next.learner = learner, package = learner$package, par.set = par.set, par.vals = par.vals, cl = "MultiFidLearner")
}

# trains the learners inside of MultiFidLearner with on task data and
# returns ????  a list splits automaticaly ??
train.MultiFidLearner = function(obj, task, subset) {
  # some shortcuts
  control = obj$par.vals$mbo.control
  learner = obj$next.learner
  
  fid.par = control$multifid.param
  fid.lvls = control$multifid.lvls
  fid.ns = as.character(fid.lvls)
  data = getTaskData(task)
  cns = colnames(data)

  if (!all(fid.lvls %in% data[[fid.par]]))
    stopf("MultiFid model has to be initialized on all values of '%s'", collapse(fid.lvls))
  models = namedList(fid.ns, NULL) #init named empty model list
  v.prev = NULL
  for (v in fid.lvls) {
    r.inds = which(data[[fid.par]] == v)
    c.names = setdiff(cns, fid.par)
    sub.task = subsetTask(task, subset = r.inds, features = c.names)
    if (is.null(v.prev)) {
      #train base model for first perf.val
      models[[as.character(v)]] =
        train(learner = learner, task = sub.task)
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
        train(learner = learner, task = sub.task)
    }
    v.prev = v
  }
  models = list(models = models, mbo.control = control)
  #FIXME: maybe use makeWrappedModel
  # makeS3Obj(c("MultiFidModel","WrappedModel"), learner = learner, models = models, task.desc = task$task.desc)
  mlr:::makeChainModel(next.model = models, cl = "MultiFidModel")
  
}

update.MultiFidModel = function(obj, task) {
  # FIXME: implement updating mechanism here later!
  learner = obj$learner
  train.MultiFidLearner(learner, task)
}

predict.MultiFidModel = function(model, task, newdata, subset = NULL) {
  # some shortcuts
  models = model$next.model$models
  fid.par = model$next.model$mbo.control$multifid.param
  fid.lvls = model$next.model$mbo.control$multifid.lvls
  fid.ns = as.character(fid.lvls)

  # we calculate a lot of unneccessary things here (for now)
  # we ignore the given perf.val in (newdata, task) and calcuate it for all
  if (missing(newdata)) {
    newdata = getTaskData(task, subset = subset)
  } else if (!is.null(subset)) {
    newdata = newdata[subset, , drop = FALSE]
  }
  fid.par.col = newdata[[fid.par]]
  fid.lvls.avail = sort(unique(fid.par.col))
  assertSubset(fid.lvls.avail, fid.lvls)
  newdata = newdata[,  setdiff(colnames(newdata) , fid.par), drop = FALSE] #remove column with fid.par
  split.inds = split(seq_row(newdata), fid.par.col)

  preds = lapply(fid.lvls.avail, function(v) {
    this.data = subset(newdata, fid.par.col == v)
    preds.each = lapply(models[fid.lvls <= v], mlr:::predict.WrappedModel, newdata = this.data)
    se = extractSubList(preds.each, c("data", "se"), simplify = FALSE)
    response = extractSubList(preds.each, c("data", "response"), simplify = FALSE)
    pred = preds.each[[1]]
    pred$data$se = Reduce('+', se)
    pred$data$response = Reduce('+', response)
    pred
  })
  names(preds) = fid.lvls.avail

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
