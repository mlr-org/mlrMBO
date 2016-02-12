if (FALSE) {
  n = 100
  x = seq(from = -3, to = 3, length.out = n)
  df = data.frame(y = x^2 + rnorm(n), .multifid.lvl = sample(1:3, n, replace = TRUE), x = x)
  task = makeRegrTask(data = df, target = "y")
  ctrl = makeMBOControl()
  ctrl = setMBOControlMultiFid(ctrl, param = ".multifid.lvl", lvls = 1:3/3)
  lrn = makeLearner("regr.km")
  learner = makeMultiFidWrapper(lrn, ctrl)
  mod = train(learner, task)
  p = predict(mod, task)
}

makeMultiFidWrapper = function(learner, control) {
  learner = checkLearner(learner)
  assertClass(control, "MBOControl")
  # FIXME: export this in mlr?
  w = mlr:::makeBaseWrapper(
    id = sprintf("%s.multifid", learner$id),
    type = learner$type,
    next.learner = learner,
    package = learner$package,
    learner.subclass = "MultiFidWrapper",
    model.subclass = "MultiFidModel"
  )
  w$mbo.control = control
  w
}

#' @export
trainLearner.MultiFidWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  control = .learner$mbo.control   # control object
  fid.lvls = control$multifid.lvls # numeric vector of levels, in ascending order, e.g. c(0.3, 0.7)
  n.lvls = length(fid.lvls)        # number of levels
  lvls = getTaskData(.task)$.multifid.lvl # integer referering to fid.lvls
  cn = setdiff(getTaskFeatureNames(.task), ".multifid.lvl") # feature names
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

  # we ignore the given perf.val in (newdata, task) and calcuate it for all
  lvls = .newdata$.multifid.lvl
  cn = setdiff(colnames(.newdata), ".multifid.lvl")
  split.inds = split(seq_along(lvls), lvls)
  lvls.inds = sort(unique(lvls))

  responses = lapply(seq_along(lvls.inds), function(i) {
    rows = split.inds[[i]]
    if (length(rows) == 0L)
      return(numeric(0L))
    sub.data = .newdata[rows, cn, drop = FALSE]
    pred = lapply(head(models, lvls.inds[i]), mlr:::predict.WrappedModel, newdata = sub.data)
    response = extractSubList(pred, c("data", "response"), simplify = FALSE)
    se = tail(pred,1)[[1]]$data$se
    cbind.data.frame(Reduce("+", response), se)
  })
  reorder = match(seq_along(lvls), unlist(split.inds, use.names = FALSE))
  res = as.matrix(do.call(rbind.data.frame, responses)[reorder,])
  res
}

#' @export
isFailureModel.MultiFidModel = function(model) {
  mods = model$learner.model$next.model
  any(vlapply(mods, isFailureModel))
}

#' @export
getLearnerProperties.MultiFidWrapper = function(learner) {
  #FIXME Not so nice workaround for the .multifid.lvl param
  #FIXME - why does only this very direct call work?
  union("numerics", mlr::getLearnerProperties(learner$next.learner))
}
