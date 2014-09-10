#FIXME: how to choose best element. with noise? without?
#FIXME: cmaes doesn't work when optimum in constraints
# Performns single objective mbo, then creates result S3 object
mboSingleObj = function(fun, par.set, design = NULL, learner, control, show.info = TRUE,
  more.args = list(), continue = NULL) {

  # initialize stopping criteria stuff
  start.time = Sys.time()
  iters = control$iters
  time.budget = control$time.budget

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

  # for normal start, we setup initial design, otherwise take stuff from continue object from disk
  if (is.null(continue)) {
    # create opt.path
    opt.path = makeMBOOptPath(par.set, control)
    # generate initial design
    extras = getExtras(ninit, NULL, control)
    generateMBODesign(design, fun, par.set, opt.path, control, show.info, oldopts, more.args, extras)
    models = namedList(control$store.model.at)
    resample.vals = namedList(control$resample.at)
  } else {
    if (!is.null(continue$mbo.result)) {
      warningf("mboContinue: No need to continue, we were already finished. Simply returning stored result.")
      return(continue$mbo.result)
    }
    opt.path = continue$opt.path
    models = continue$models
    resample.vals = continue$resample.vals
  }

  # set up initial mbo task + train model
  rt = makeMBOSingleObjTask(par.set, opt.path, control)
  model = train(learner, rt)

  # save some stuff for iter 0, not necessary if we continue
  if (is.null(continue)) {
    if (0L %in% control$store.model.at)
      models[["0"]] = model
    if (0L %in% control$resample.model.at) {
      r = resample(learner, rt, control$resample.desc, measures = control$resample.measures)
      resample.vals[["0"]] = r$aggr
    }
    saveStateOnDisk(0L, fun, learner, par.set, opt.path, control, show.info, more.args,
      models, resample.vals, NULL)
  }

  # if we are restarting from a save file, we possibly start in a higher iteration
  loop = max(getOptPathDOB(opt.path)) + 1L

  repeat {
    # propose new points and evaluate target function
    prop = proposePoints(model, par.set, control, opt.path, iter = loop)
    prop.points = prop$prop.points
    crit.vals = prop$crit.vals

    extras = getExtras(nrow(prop$prop.points), prop, control)
    evalProposedPoints(loop, prop.points, par.set, opt.path, control,
      fun, learner, show.info, oldopts, more.args, extras)

    rt = makeMBOSingleObjTask(par.set, opt.path, control)
    model = train(learner, rt)
    if (loop %in% control$store.model.at)
      models[[as.character(loop)]] = model
    if (loop %in% control$resample.at) {
      r = resample(learner, rt, control$resample.desc, measures = control$resample.measures)
      resample.vals[[as.character(loop)]] = r$aggr
    }
    saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args,
      models, resample.vals, NULL)
    loop = loop + 1L
    terminate = shouldTerminate(iters, loop, time.budget, start.time)
    if (terminate >= 0)
      break
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
    extras = getExtras(fevals, NULL, control)
    ys = evalTargetFun(fun, par.set, loop, xs, opt.path, control, show.info, oldopts, more.args, extras)
    best$y = mean(ys)
  }
  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  res = makeS3Obj(c("MBOSingleObjResult", "MBOResult"),
    x = best$x,
    y = as.numeric(best$y), # strip name
    best.ind = final.index,
    opt.path = opt.path,
    resample = resample.vals,
    convergence = terminate,
    models = models
  )

  # make sure to save final res on disk
  saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args,
    models, resample.vals, res)

  return(res)
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


