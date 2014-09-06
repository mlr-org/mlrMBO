# Performce the MSPOT algorithm with flexible infil crit for multicrit optim
mboMSPOT = function(fun, par.set, design = NULL, learner, control, show.info = TRUE,
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
  ninit = if(is.null(design)) control$init.design.points else nrow(design)

  # helper to get extras-list for opt.path logging
  getExtras = function(crit.vals, error.model, lambdas) {
    n = length(crit.vals)
    crit.vals = unlist(crit.vals)
    exs = vector("list", n)
    for (i in 1:n) {
      ex = list(crit.vals[i], error.model = error.model)
      names(ex)[1] = crit
      exs[[i]] = ex
    }
    return(exs)
  }

  # for normal start, we setup initial design, otherwise take stuff from continue object from disk
  if (is.null(continue)) {
    # create opt.path
    opt.path = makeMBOOptPath(par.set, control)
    # generate initial design
    generateMBODesign(design, fun, par.set, opt.path, control, show.info, oldopts, more.args,
      extras = getExtras(crit.vals = rep(NA_real_, ninit), error.model = NA_character_, lambdas = rep(NA_real_, ninit)))
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

  # save some stuff for iter 0, not necessary if we continue
  if (is.null(continue)) {
    if (0L %in% control$store.model.at)
      models[["0"]] = model
    if (0L %in% control$resample.model.at) {
      r = resample(learner, rt, control$resample.desc, measures = control$resample.measures)
      resample.vals[["0"]] = r$aggr
    }
    saveStateOnDisk(0L, fun, learner, par.set, opt.path, control, show.info, more.args, models, resample.vals, NULL)
  }

  # if we are restarting from a save file, we possibly start in a higher iteration
  loop = max(getOptPathDOB(opt.path)) + 1L

  repeat {
    rt = makeMBOMultiObjTasks(par.set, opt.path, control)
    y.models = lapply(rt, train, learner = learner)

    # propose new points and evaluate target function
    prop = proposePointsMSPOT(y.models, par.set, control, opt.path)
    prop.points = prop$prop.points
    crit.vals = prop$crit.vals

    extras = getExtras(crit.vals, prop$error.model, attr(prop.points, "multipoint.lcb.lambdas"))
    evalProposedPoints(loop, prop.points, par.set, opt.path, control,
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
