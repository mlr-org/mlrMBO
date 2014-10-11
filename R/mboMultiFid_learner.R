makeMultiFidWrapper = function(learner, control) {
  learner = checkLearner(learner, type = "regr")
  assertClass(control, "MBOControl")
  #if (learner$predict.type != "se")
  #  stop("Predict type of the basic learner must be 'se'.")
  id = paste(learner$id, "multifid", sep = ".")
  packs = learner$package
  w = mlr:::makeBaseWrapper(id, learner, packs, cl = "MultiFidWrapper")
  w$mbo.control = control
  return(w)
}

#' @export
trainLearner.MultiFidWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  # some shortcuts
  control = .learner$mbo.control
  fid.par = control$multifid.param
  fid.lvls = control$multifid.lvls
  fid.ns = as.character(fid.lvls)
  data = getTaskData(.task)
  cns = colnames(data)
  if (!all(fid.lvls %in% data[[fid.par]]))
    stopf("MultiFid model has to be initialized on all values of '%s'", collapse(fid.lvls))
  models = namedList(fid.ns, NULL) #init named empty model list
  v.prev = NULL
  for (v in fid.lvls) {
    r.inds = which(data[[fid.par]] == v)
    c.names = setdiff(cns, fid.par)
    sub.task = subsetTask(.task, subset = r.inds, features = c.names)
    if (is.null(v.prev)) {
      #train base <F10>model for first perf.val
      models[[as.character(v)]] =
        train(learner = .learner$next.learner, task = sub.task)
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
        train(learner = .learner$next.learner, task = sub.task)
    }
    v.prev = v
  }
  mlr:::makeChainModel(next.model = models, cl = "MultiFidModel")
}

#' @export
predictLearner.MultiFidWrapper = function(.learner, .model, .newdata, ...) {
  # some shortcuts
  models = .model$learner.model$next.model
  fid.par = .learner$mbo.control$multifid.param
  fid.lvls = .learner$mbo.control$multifid.lvls
  fid.ns = as.character(fid.lvls)

  # we calculate a lot of unneccessary things here (for now)
  # we ignore the given perf.val in (newdata, task) and calcuate it for all
  fid.par.col = .newdata[[fid.par]]
  fid.lvls.avail = sort(unique(fid.par.col))
  assertSubset(fid.lvls.avail, fid.lvls)
  .newdata = .newdata[,  setdiff(colnames(.newdata) , fid.par), drop = FALSE] #remove column with fid.par
  split.inds = split(seq_row(.newdata), fid.par.col)

  preds = lapply(fid.lvls.avail, function(v) {
    this.data = subset(.newdata, fid.par.col == v)
    preds.each = lapply(models[fid.lvls <= v], mlr:::predict.WrappedModel, newdata = this.data)
    # se = extractSubList(preds.each, c("data", "se"), simplify = FALSE)
    response = extractSubList(preds.each, c("data", "response"), simplify = FALSE)
    pred = preds.each[[1]]
    # pred$data$se = Reduce('+', se)
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
  pred.data$response
}


#' @export
makeWrappedModel.MultiFidWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  addClasses(x, "MultiFidModel")
}

#' @export
isFailureModel.MultiFidModel = function(model) {
  mods = model$learner.model$next.model
  isit = vlapply(mods, isFailureModel)
  return(any(isit))
}

#' @export
print.MultiFidModel = function(x, ...) {
  s = capture.output(mlr:::print.WrappedModel(x))
  u = sprintf("Multifid Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}


