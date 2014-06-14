#FIXME: retrain kriging faster

#FIXME: how to choose best element. with noise? without?
#FIXME: different name for final evals in output (not last step number)

#FIXME: cmaes doesn't work when optimum in constraints


# performns single objective mbo, then creates result S3 object

mboSingleObj = function(fun, par.set, design = NULL, learner, control, show.info = TRUE, more.args = list()) {


  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # shortcut names
  y.name = control$y.name
  crit = control$infill.crit
  islcb = (control$propose.points > 1L && control$multipoint.method == "lcb")
  ninit = if(is.null(design)) control$init.design.points else nrow(design)
  fevals = control$final.evals

  # create opt.path
  opt.path = makeMBOOptPath(par.set, control)

  # helper to get extras-list for opt.path logging
  getExtras = function(crit.vals, model.fail, lambdas) {
    n = length(crit.vals)
    exs = vector("list", n)
    for (i in 1:n) {
      ex = list(crit.vals[i], .model.fail = model.fail)
      names(ex)[1] = crit
      if (islcb)
        ex$multipoint.lcb.lambda = lambdas[i]
      exs[[i]] = ex
    }
    return(exs)
  }

  # generate initial design
  mbo.design = generateMBODesign(design, fun, par.set, opt.path, control, show.info, oldopts, more.args,
    extras = getExtras(crit.vals = rep(NA_real_, ninit), model.fail = NA_character_, lambdas = rep(NA_real_, ninit)))

  # set up initial mbo task
  rt = makeMBOSingleObjTask(par.set, opt.path, control)
  model = train(learner, rt)

  models = namedList(control$save.model.at)
  res.vals = namedList(control$resample.at)

  if (0L %in% control$save.model.at) {
    models[["0"]] = model
  }

  if (0L %in% control$resample.model.at) {
    r = resample(learner, rt, control$resample.desc, measures = control$resample.measures)
    res.vals[["0"]] = r$aggr
  }

  saveStateOnDisk(0L, fun, learner, par.set, opt.path, control, show.info, more.args)

  # do the mbo magic
  # if we are restarting from a save file, we possibly start in a higher iteration
  start.loop = max(getOptPathDOB(opt.path)) + 1
  for (loop in start.loop:control$iters) {

    # propose new points and evaluate target function
    prop = proposePoints(model, par.set, control, opt.path)
    prop.points = prop$prop.points
    crit.vals = prop$crit.vals

    extras = getExtras(crit.vals, prop$model.fail, attr(prop.points, "multipoint.lcb.lambdas"))
    evalProposedPoints(loop, prop.points, par.set, opt.path, control,
      fun, learner, show.info, oldopts, more.args, extras)

    rt = makeMBOSingleObjTask(par.set, opt.path, control)
    model = train(learner, rt)
    if (loop %in% control$save.model.at)
      models[[as.character(loop)]] = model
    if (loop %in% control$resample.at) {
      r = resample(learner, rt, control$resample.desc, measures = control$resample.measures)
      res.vals[[as.character(loop)]] = r$aggr
    }
    saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args)
  }

  design = getTaskData(rt, target.extra = TRUE)$data
  final.index = chooseFinalPoint(fun, par.set, model, opt.path, y.name, control)
  best = getOptPathEl(opt.path, final.index)
  x = best$x
  y = best$y

  if (fevals > 0L) {
    showInfo(show.info, "Performing %i final evals", fevals)
    # do some final evaluations and compute mean of target fun values
    xs = replicate(fevals, best$x, simplify = FALSE)
    ys = evalTargetFun(fun, par.set, loop, xs, opt.path, control, show.info, oldopts, more.args,
      extras = getExtras(crit.vals = rep(NA_real_, fevals), model.fail = NA_character_, lambdas = rep(NA_real_, fevals)))
    best$y = mean(ys)
  }
  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  # make sure to strip name of y
  makeS3Obj(c("MBOSingleObjResult", "MBOResult"),
    x = best$x,
    # strip name
    y = as.numeric(best$y),
    opt.path = opt.path,
    resample = res.vals,
    models = models
  )
}

#' @export
print.MBOResult = function(x, ...) {
  op = x$opt.path
  catf("Recommended parameters:")
  catf(paramValueToString(op$par.set, x$x))
  catf("Objective: %s = %.3f\n", op$y.names[1], x$y)
  catf("Optimization path")
  n1 = sum(op$env$dob == 0)
  n2 = length(op$env$dob) - n1
  catf("%i + %i entries in total, displaying last 10 (or less):", n1, n2)
  print(tail(as.data.frame(op), 10))
}


