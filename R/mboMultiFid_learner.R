if (FALSE) {
  n = 100
  x = seq(from = -3, to = 3, length.out = n)
  df = data.frame(y = x^2 + rnorm(n), .multifid.lvl = sample(1:3, n, replace = TRUE), x = x)
  task = makeRegrTask(data = df, target = "y")
  ctrl = makeMBOControl()
  ctrl = setMBOControlMultiFid(ctrl, param = ".multifid.lvl", lvls = 1:3/3)
  lrn = makeLearner("regr.randomForest")
  lrn = setPredictType(lrn, "se")
  learner = makeMultiFidWrapper(lrn, ctrl)
  mod = train(learner, task)
}

makeMultiFidWrapper = function(learner, control) {
  learner = checkLearner(learner, type = "regr")
  assertClass(control, "MBOControl")
  # FIXME: export this in mlr?
  w = mlr:::makeBaseWrapper(
    id = sprintf("%s.multifid", learner$id),
    next.learner = learner,
    package = learner$package,
    cl = "MultiFidWrapper")
  w$mbo.control = control
  # FIXME: this is a ugly hack
  return(addProperties(w, c("numerics")))
}

#' @export
trainLearner.MultiFidWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  control = .learner$mbo.control   # control object
  # fid.par = control$multifid.param # name of column in task, e.g. ".fidelity.lvl"
  fid.par = ".multifid.lvl"        # we agreed on this internal column name
  fid.lvls = control$multifid.lvls # numeric vector of levels, in ascending order, e.g. c(0.3, 0.7)
  n.lvls = length(fid.lvls)        # number of levels
  lvls = getTaskData(.task)[[fid.par]] # integer referering to fid.lvls
  cn = setdiff(getTaskFeatureNames(.task), fid.par) # feature names
  if (!all(seq_len(n.lvls) %in% lvls))
    stopf("MultiFid model has to be initialized on all values of '%s'", collapse(fid.lvls))

  models = vector("list", length(fid.lvls))
  models[[1L]] = train(
    learner = .learner$next.learner,
    task = subsetTask(.task, subset = which(lvls == 1L), features = cn)
  )

  if (n.lvls > 1L) {
    tn = getTaskTargetNames(.task)
    for (i in seq(from = 2L, to = n.lvls)) {
      # train deltas
      # calculcate surrogate values of one level below
      sub.task = subsetTask(.task, subset = which(lvls == i), features = cn)
      preds = lapply(head(models, i-1L), predict, task = sub.task)
      preds = extractSubList(xs = preds, element = c("data", "response"), simplify = "cols")

      y = getTaskTargets(sub.task) - rowSums(preds)
      sub.task$env$data[[tn]] = y
      #FIXME: in case we have nearly a perfect constant shift between levels, kriging of course crashes here....
      models[[i]] = train(learner = .learner$next.learner, task = sub.task)
    }
  }
  # FIXME: export this in mlr?
  mlr:::makeChainModel(next.model = models, cl = "MultiFidModel")
}

#' @export
predictLearner.MultiFidWrapper = function(.learner, .model, .newdata, ...) {
  models = .model$learner.model$next.model
  control = .learner$mbo.control   # control object
  # fid.par = control$multifid.param # name of column in task, e.g. ".multifid.lvl"
  fid.par = ".multifid.lvl"        # we agreed on this internal column name
  fid.lvls = control$multifid.lvls # numeric vector of levels, in ascending order, e.g. c(0.3, 0.7)

  # we ignore the given perf.val in (newdata, task) and calcuate it for all
  lvls = .newdata[[fid.par]]
  cn = setdiff(colnames(.newdata), fid.par)
  split.inds = split(seq_along(lvls), lvls)

  responses = lapply(seq_along(fid.lvls), function(i) {
    rows = split.inds[[i]]
    if (length(rows) == 0L)
      return(numeric(0L))
    sub.data = .newdata[rows, cn, drop = FALSE]
    response = lapply(head(models, i), mlr:::predict.WrappedModel, newdata = sub.data)
    response = extractSubList(response, c("data", "response"), simplify = FALSE)
    Reduce("+", response)
  })
  unlist(responses, use.names = FALSE)[match(seq_along(lvls), unlist(split.inds, use.names = FALSE))]
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
  # FIXME: export this in mlr?
  s = capture.output(mlr:::print.WrappedModel(x))
  u = sprintf("Multifid Learner: %s", class(x$learner$next.learner)[1L])
  s = append(s, u, 1L)
  lapply(s, catf)
}
