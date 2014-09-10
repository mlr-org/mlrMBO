#  Optimizes a multicrit optimization problem with the SMS EGO algo

mboMultiCritSMS = function(fun, par.set, design = NULL, learner, control, show.info = TRUE,
  more.args = list(), continue = NULL) {

  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # termination
  start.time = Sys.time()
  iters = control$iters
  time.budget = control$time.budget

  # shortcut names
  ninit = if(is.null(design)) control$init.design.points else nrow(design)
  crit = control$infill.crit

  # for normal start, we setup initial design, otherwise take stuff from continue object from disk
  if (is.null(continue)) {
    opt.path = makeMBOOptPath(par.set, control)
    extras = getExtras(ninit, NULL, control)
    generateMBODesign(design, fun, par.set, opt.path, control, show.info, oldopts, more.args, extras)
    models = namedList(control$store.model.at)
    saveStateOnDisk(0L, fun, learner, par.set, opt.path, control, show.info, more.args, models, NULL, NULL)
  } else {
    if (!is.null(continue$mbo.result)) {
      warningf("mboContinue: No need to continue, we were already finished. Simply returning stored result.")
      return(continue$mbo.result)
    }
    opt.path = continue$opt.path
    models = continue$models
  }

  # new control for scalar soo iteration, always minimize and propose 1 point
  ctrl2 = control
  ctrl2$propose.points = 1L
  ctrl2$minimize = TRUE

  # if we are restarting from a save file, we possibly start in a higher iteration
  loop = max(getOptPathDOB(opt.path)) + 1L
  repeat {
    rt = makeMBOMultiObjTasks(par.set, opt.path, control)
    y.models = lapply(rt, train, learner = learner)
    if (loop %in% control$store.model.at)
      models[[as.character(loop)]] = y.models

    # propose new points and evaluate target function
    prop = proposePoints(y.models, par.set, control, opt.path, iter = loop)
    extras = getExtras(nrow(prop$prop.points), prop, control)
    evalProposedPoints(loop, prop$prop.points, par.set, opt.path, control,
      fun, learner, show.info, oldopts, more.args, extras)

    saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args, models, resample.vals, NULL)
    loop = loop + 1L
    terminate = shouldTerminate(iters, loop, time.budget, start.time)
    if (terminate >= 0)
      break
  }
  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  res = makeMBOMultiCritResult(opt.path, terminate, models)

  # make sure to save final res on disk
  saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args, models, NULL, res)

  return(res)
}



