#  Optimizes a multicrit optimization problem with the parego algorithm

mboParEGO = function(fun, par.set, design = NULL, learner, control, show.info = TRUE,
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
  ninit = if (is.null(design)) control$init.design.points else nrow(design)
  crit = control$infill.crit


  # Calculate all possible weight vectors and save them
  all.possible.weights = combWithSum(control$parego.s, control$number.of.targets) / control$parego.s
  # rearrange them a bit - we want to have the margin weights on top of the matrix
  # tricky: all margin weights have maximal variance
  vars = apply(all.possible.weights, 1, var)
  all.possible.weights = rbind(diag(control$number.of.targets), all.possible.weights[!vars == max(vars),])

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
  ctrl2$number.of.targets = 1L
  ctrl2$minimize = TRUE

  # if we are restarting from a save file, we possibly start in a higher iteration
  loop = max(getOptPathDOB(opt.path)) + 1L
  repeat {
    # scalarize + train + propose
    scalar = makeScalarTasks(par.set, opt.path, control, all.possible.weights)
    new.mods = lapply(scalar$tasks, train, learner = learner)
    if (loop %in% control$store.model.at)
      models[[as.character(loop)]] = new.mods
    props = lapply(new.mods, proposePoints, par.set = par.set,
      control = ctrl2, opt.path = opt.path)
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE))
    prop = list(
      prop.points = prop.points,
      crit.vals = extractSubList(props, "crit.vals"),
      errors.model = extractSubList(props, "errors.model")
    )
    extras = getExtras(nrow(prop$prop.points), prop, control, scalar$weights)
    evalProposedPoints(loop, prop.points, par.set, opt.path, control,
      fun, learner, show.info, oldopts, more.args, extras)
    saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args, models, NULL, NULL)
    loop = loop + 1L
    terminate = shouldTerminate(iters, loop, time.budget, start.time)
    if (terminate >= 0)
      break
  }

  pareto.inds = getOptPathParetoFront(opt.path, index = TRUE)

  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  res = makeMBOMultiCritResult(opt.path, terminate, models)

  # make sure to save final res on disk
  saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args,  models, NULL, res)

  return(res)
}

