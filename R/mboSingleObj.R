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
  ninit = control$init.design.points

  # create extra.par.set and opt.path
  extra.par.set = if (islcb) {
    makeParamSet(
      makeNumericParam(crit),
      makeNumericParam("multipoint.lcb.lambda")
    )
  } else {
    makeParamSet(
      makeNumericParam(crit)
    )
  }
  opt.path = makeOptPathDF(
    par.set, include.error.message = TRUE, include.exec.time = TRUE,
    y.names = control$y.name,
    minimize = control$minimize,
    extra.par.set = extra.par.set
  )

  # helper to get extras-list for opt.path logging
  getExtras = function(crit.vals, lambdas) {
    if (!islcb)
      lapply(crit.vals, namedList, name = crit)
    else
     Map(function(v, l) setNames(list(v, l), c(crit, "multipoint.lcb.lambda")), crit.vals, lamdas)
  }

  # generate initial design
  mbo.design = generateMBODesign(design, fun, par.set, opt.path, control, show.info, oldopts, more.args,
    extras = getExtras(rep(NA_real_, ninit), rep(NA_real_, ninit)))

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

  # Save on disk?
  if (0 %in% control$save.on.disk.at) {
    save(list = c("opt.path", "fun", "par.set", "learner", "control", "show.info", "more.args"),
      file = control$save.file.path)
  }

  # do the mbo magic
  # if we are restarting from a save file, we possibly start in a higher iteration
  start.loop = max(getOptPathDOB(opt.path)) + 1
  for (loop in start.loop:control$iters) {

    # propose new points and evaluate target function
    prop.design = proposePoints(model, par.set, control, opt.path)
    prop.points = prop.design$prop.points
    prop.points.crit.values = prop.design$prop.points.crit.values

    # handle lambdas for this method
    if (islcb) {
      multipoint.lcb.lambdas = rbind(multipoint.lcb.lambdas, attr(prop.points, "multipoint.lcb.lambdas"))
      attr(prop.points, "multipoint.lcb.lambda") =  NULL
    }

    xs = dfRowsToList(prop.points, par.set)
    xs = lapply(xs, repairPoint, par.set = par.set)
    ys = evalTargetFun(fun, par.set, loop, xs, opt.path, control, show.info, oldopts, more.args,
      extras = getExtras(prop.points.crit.values, multipoint.lcb.lambdas))

    rt = makeMBOTask(as.data.frame(opt.path, discretes.as.factor = TRUE), par.set, control)
    model = train(learner, rt)
    if (loop %in% control$save.model.at)
      models[[as.character(loop)]] = model
    if (loop %in% control$resample.at) {
      r = resample(learner, rt, control$resample.desc, measures = control$resample.measures)
      res.vals[[as.character(loop)]] = r$aggr
    }
    # Save on disk?
    if (loop %in% control$save.on.disk.at) {
      save(list = c("opt.path", "fun", "par.set", "learner", "control", "show.info", "more.args"),
        file = control$save.file.path)
    }
  }

  design = getTaskData(rt, target.extra = TRUE)$data
  final.index = chooseFinalPoint(fun, par.set, model, opt.path, y.name, control)
  best = getOptPathEl(opt.path, final.index)
  x = best$x
  y = best$y

  if (control$final.evals > 0L) {
    if (show.info)
      messagef("Performing %i final evals", control$final.evals)
    # do some final evaluations and compute mean of target fun values
    xs = replicate(control$final.evals, best$x, simplify = FALSE)
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, more.args)
    ys = evals$ys
    times = c(times, evals$times)
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


