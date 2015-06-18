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

  tasks = getTuningStateTasks(tuningState)
  tr = getTuningStateModels(tuningState)
  
  # small helper for learner resampling
  # if we have multiple tasks, return a list, otherwise singleton result
  doResample = function(tasks) {
    if (length(tasks) == 1L)
      resample(learner, tasks[[1L]], control$resample.desc, measures = control$resample.measures, show.info = FALSE)
    else
      lapply(tasks, resample, learner = learner, resampling = control$resample.desc,
        measures = control$resample.measures, show.info = FALSE)
  }

  # save some stuff for iter 0, not necessary if we continue
  if (is.null(continue)) {
    if (0L %in% control$store.model.at)
      stored.models[["0"]] = if (length(tr$models) == 1L) tr$models[[1L]] else tr$models
    if (0L %in% control$resample.model.at)
      resample.results[["0"]] = doResample(tasks)
    saveStateOnDisk(0L, fun, learner, par.set, opt.path, control, show.info, more.args,
      stored.models, resample.results, NULL)
  }

  repeat {
    # propose new points and evaluate target function
    prop = proposePoints(tasks, tr$models, par.set, control, opt.path, iter = loop)
    # drop proposed points, which are too close to design points
    if (control$filter.proposed.points) {
      prop = filterProposedPoints(prop, opt.path, par.set, control)
    }
    extras = getExtras(nrow(prop$prop.points), prop, tr$train.time, control)
    evalProposedPoints(loop, prop$prop.points, par.set, opt.path, control,
      fun, learner, show.info, oldopts, more.args, extras)

    #FIXME: why do we do the expansive operation of training here, if we might break at the end?
    # dont we waste useless time for the final iter?
    # ok we might use the model for choosing the final point....
    # but what if not?

    # train models
    tasks = makeTasks(par.set, opt.path, algo.init, control)
    tr = trainModels(learner, tasks, control)

    # store models + resample + store on disk
    if (loop %in% control$store.model.at)
      stored.models[[as.character(loop)]] = if (length(tr$models) == 1L) tr$models[[1L]] else tr$models
    if (loop %in% control$resample.at)
      resample.results[[as.character(loop)]] = doResample(tasks)
    saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args, stored.models,
      resample.results, NULL)

    setTuningStateLoop(tuningState)
    terminate = shouldTerminate.TuningState(tuningState)
    if (terminate >= 0)
      break
  }
  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

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
  saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args, stored.models,
    resample.results, res)

  return(res)
}


