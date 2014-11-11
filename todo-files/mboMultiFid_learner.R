# Notes:
# * Levels should be sticked to the task or learner or whatever, not be a feature in the task
# * Be super careful with subsetting! Especially because we cannot rely on subsetTask alone,
#   we need to take care of level subsetting ourselves.
# * Levels are numeric. This is bad. Sees FIXMEs below.
# * Best would be to have a MultiFidTask which inherits from a RegrTask?
#   Would be so much easier if we had already the new mlr tasks!
if (FALSE) {
  library(devtools)
  library(checkmate)
  load_all("~/vcs/mlr")
  load_all("~/vcs/mlrMBO")
  set.seed(1)
  data = data.frame(
    lvl = factor(sample(c(0.3, 0.9), 20, replace = TRUE), ordered = FALSE, levels = c(0.3, .9)),
    x = seq(0, 2*pi, length.out = 20))
  data$y = sin(data$x) + rnorm(20)

  ctrl = makeMBOControl()
  ctrl = setMBOControlMultiFid(ctrl, param = "lvl", lvls = c(0.3, 0.9))
  task = makeRegrTask(id = "testtask", data = data, target = "y")
  lrn = makeLearner("regr.km", config = list(show.learner.output = FALSE))
  lrn = setPredictType(lrn, "se")
  lrn = makeMultiFidWrapper(lrn, ctrl)
  # train(lrn, task)
}

makeMultiFidWrapper = function(learner, control) {
  learner = checkLearner(learner, type = "regr")
  assertClass(control, "MBOControl")
  #if (learner$predict.type != "se")
  #  stop("Predict type of the basic learner must be 'se'.")
  w = mlr:::makeBaseWrapper(
    id = sprintf("%s.multifid", learner$id),
    next.learner = learner,
    package = learner$package,
    cl = "MultiFidWrapper")
  w$mbo.control = control
  return(w)
}

#' @export
trainLearner.MultiFidWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  # some shortcuts
  control = .learner$mbo.control
  fid.par = control$multifid.param
  fid.lvls = control$multifid.lvls
  .task = subsetTask(.task, subset = .subset)
  data = getTaskData(.task)
  cns = colnames(data)
  # FIXME: this is numerically unstable!
  if (!all(fid.lvls %in% data[[fid.par]]))
    stopf("MultiFid model has to be initialized on all values of '%s'", collapse(fid.lvls))
  models = vector("list", length(fid.lvls))
  c.names = setdiff(cns, fid.par)
  tn = getTargetNames(.task)
  for (i in seq_along(models)) {
    r.inds = which(data[[fid.par]] == v)
    sub.task = subsetTask(.task, subset = r.inds, features = c.names)
    if (i == 1L) {
      models[[i]] = train(learner = .learner$next.learner, task = sub.task)
    } else {
      #train deltas
      #calculcate surrogate values of one level below (usage of predict.MultiFid would be awsome here)
      preds = lapply(models[seq_len(i -1L)], predict, task = sub.task)
      preds.response = extractSubList(xs = preds, element = c("data", "response"), simplify = FALSE)
      preds.response = asMatrixCols(preds.response)
      traindata = getTaskData(sub.task)
      traindata[[tn]] = traindata[[tn]] - rowSums(preds.response)
      sub.task = mlr:::changeData(task = sub.task, data = traindata)
      #FIXME: in case we have nearly a perfect constant shift between levels, kriging of course crashes here....
      models[[i]] = train(learner = .learner$next.learner, task = sub.task)
    }
  }
  mlr:::makeChainModel(next.model = models, cl = "MultiFidModel")
}

#' @export
predictLearner.MultiFidWrapper = function(.learner, .model, .newdata, ...) {
  # some shortcuts
  models = .model$learner.model$next.model
  fid.par = .learner$mbo.control$multifid.param
  fid.lvls = .learner$mbo.control$multifid.lvls

  # we calculate a lot of unneccessary things here (for now)
  # we ignore the given perf.val in (newdata, task) and calcuate it for all
  fid.par.col = .newdata[[fid.par]]
  # FIXME: this is numericaly unstable
  fid.lvls.avail = sort(unique(fid.par.col))
  assertSubset(fid.lvls.avail, fid.lvls)
  .newdata = .newdata[,  setdiff(colnames(.newdata) , fid.par), drop = FALSE] # remove column with fid.par
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
  for (i in seq_along(fid.lvls)) {
    if (!is.null(split.inds[[i]]))
      pred.data[split.inds[[i]], ] = preds[[i]]$data
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
  any(vlapply(mods, isFailureModel))
}

#' @export
print.MultiFidModel = function(x, ...) {
  s = capture.output(mlr:::print.WrappedModel(x))
  u = sprintf("Multifid Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}
