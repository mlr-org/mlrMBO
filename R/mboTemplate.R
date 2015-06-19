# magic mboTemplate - in this function the mbo magic for all our mbo approaches
# does happen - model fitting und point proposal in a generall way. the respective
# mbo algorithms differ in the subfunctions.

# continue - do we continue an already started (and aborted) mbo run or is this
#            a fresh one?

mboTemplate = function(fun, par.set, design = NULL, learner, control, show.info = TRUE,
  more.args = list(), continue = NULL) {

  tuningProblem = makeTuningProblem(
    fun = fun, 
    par.set = par.set, 
    design = design,
    learner = learner,
    control = control,
    show.info = show.info,
    more.args = more.args)

  if (is.null(continue)) {
    tuningState = makeTuningState(tuningProblem)
  } else {
    #FIXME: What restart options would we like
    tuningState = loadTuningState(tuningProblem)
  }

  setTuningStateLoop(tuningState)

  repeat {
    prop = proposePoints.TuningState(tuningState)
    evalProposedPoints.TuningState(tuningState, prop)
    setTuningStateLoop(tuningState)
    terminate = shouldTerminate.TuningState(tuningState)
    if (terminate >= 0)
      break
  }

  fevals = control$final.evals
  if (control$number.of.targets == 1L) {
    final.index = chooseFinalPoint(fun = fun, opt.path = opt.path, model = tr$models[[1L]], task = tasks[[1]], control = control)
    if (fevals > 0L) {
      # do some final evaluations and compute mean of target fun values
      showInfo(show.info, "Performing %i final evals", fevals)
      best = getOptPathEl(opt.path, final.index)
      xs = replicate(fevals, best$x, simplify = FALSE)
      extras = getExtras(fevals, NULL, NA_real_, control)
      ys = evalTargetFun(fun, par.set, loop, xs, opt.path, control, show.info, oldopts, more.args, extras)
      best$y = mean(ys)
    }
    res = makeMBOSingleObjResult(final.index, opt.path, resample.results, terminate,
      stored.models, control)
  } else {
    res = makeMBOMultiCritResult(opt.path, terminate, stored.models, control)
  }
  
  # make sure to save final res on disk
  saveTuningStateNow(tuningState)

  # restore mlr configuration
  configureMlr(on.learner.error = getTuningProblemOldopts(tuningProblem)[["ole"]], show.learner.output = getTuningProblemOldopts(tuningProblem)[["slo"]])

  return(res)
}


