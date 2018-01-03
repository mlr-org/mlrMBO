getOptStateOptProblem = function(opt.state) {
  opt.state$opt.problem
}

getOptStateRandomSeed = function(opt.state) {
  opt.state$random.seed
}

getOptStateOptResult = function(opt.state) {
  opt.state$opt.result
}

getOptStateModels = function(opt.state) {
  if (is.null(opt.state$models) || getOptStateLoop(opt.state) != opt.state$models.loop) {
    opt.problem = getOptStateOptProblem(opt.state)
    models = trainModels(
      learner = getOptProblemLearner(opt.problem),
      tasks = getOptStateTasks(opt.state),
      control = getOptProblemControl(opt.problem))
    setOptStateModels(opt.state, models)
  } else {
    models = opt.state$models
  }
  models
}

getOptStateTasks = function(opt.state, predictive = FALSE) {
  if (is.null(opt.state$tasks) || getOptStateLoop(opt.state) != opt.state$tasks.loop) {
    tasks = makeTasks(opt.state)
    setOptStateTasks(opt.state, tasks)
  } else {
    tasks = opt.state$tasks
  }
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  if (predictive && !is.null(getOptProblemDriftParam(opt.problem)) && control$conceptdrift.learn.drift) {
    fixed.x = getOptStateFixedLearnableParam(opt.state)
    tasks = lapply(tasks, function(z) {
      data = z$env$data
      data[[getOptProblemDriftParam(opt.problem)]] = fixed.x
      z = mlr:::changeData(task = z, data = data)
      return(z)
    })
  }
  tasks
}

getOptStateTimeModel = function(opt.state) {
  opt.path = getOptStateOptPath(opt.state)
  time.model = opt.state$time.model
  exec.times = getOptPathExecTimes(opt.path)
  if (is.null(time.model) || getTaskSize(time.model) != length(na.omit(exec.times))) {
    opt.problem = getOptStateOptProblem(opt.state)
    opt.path = getOptStateOptPath(opt.state)
    time.task = cbind(getOptPathX(opt.path), exec.time = getOptPathExecTimes(opt.path))
    time.task = time.task[!is.na(time.task$exec.time), ]
    time.task = makeRegrTask(id = "time.task", data = time.task, target = "exec.time")
    time.model = train(learner = getOptProblemLearner(opt.problem), task = time.task)
    setOptStateTimeModel(opt.state, time.model)
  }
  time.model
}

getOptStateLoop = function(opt.state) {
  opt.state$loop
}

getOptStateLoopStarttime = function(opt.state) {
  opt.state$loop.starttime
}

getOptStateTimeUsed = function(opt.state) {
  opt.state$time.used
}

getOptStateOptPath = function(opt.state) {
  #opt.problem = getOptStateOptProblem(opt.state)
  #control = getOptProblemControl(opt.problem)
  #if (!is.null(control$conceptdrift.window.function)) {
  #  res = control$conceptdrift.window.function(opt.state$opt.path)
  #  assertClass(res, "OptPathNg")
  #  #browser()
  #  return(res)
  #} else {
    opt.state$opt.path
  #}
}

getOptStateTimeLastSaved = function(opt.state) {
  opt.state$time.last.saved
}

getOptStateShouldSave = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  getOptStateLoop(opt.state) %in% control$save.on.disk.at ||
    difftime(Sys.time(), getOptStateTimeLastSaved(opt.state), units = "secs") > control$save.on.disk.at.time

}

# @param unify [\code{logical(1)}]
#   Defines if in the case of multi-objective optimization we shoud try to make
#  the output similar to the result of the normal optimization.
getOptStateFinalPoints = function(opt.state, unify = FALSE) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)

  if (control$n.objectives == 1L) {
    getBestOp = function(ind) {
      list(
        x = getOptPathX(opt.path)[ind, , drop = FALSE],
        y = getOptPathY(opt.path)[ind],
        best.ind = ind
      )
    }
    if (control$final.method == "last.proposed") {
      getBestOp(getOptPathLength(opt.path))
    } else if (control$final.method == "best.true.y") {
      getBestOp(getOptPathBestIndex(opt.path, ties = "random"))
    } else if (control$final.method == "best.predicted") {
      browser()
      maximize.mult = ifelse(control$minimize, 1, -1)
      model = getOptStateModels(opt.state)$models[[1L]]
      task = getOptStateTasks(opt.state, predictive = TRUE)[[1]]
      pred = predict(model, task = task)
      best.ind = which(rank(maximize.mult * pred$data$response, ties.method = "min") == 1L)
      # if we have ties the model might give constant predictions so we use the best y from the observations
      if (length(best.ind) > 1) {
        sub.best.ind = which(rank(maximize.mult * getOptPathY(opt.path)[best.ind], ties.method = "random") == 1L)
        best.ind = best.ind[sub.best.ind]
      } else if (contrl$fix.first.iter && getOptStateLoop(opt.state) < 2){
        # sometimes the first model does not work reliable
        best.ind = which.min(maximize.mult * pred$data$truth)
      }
      getBestOp(best.ind)
    } else if (control$final.method == "predict") {
      # FIXME: Untested, just an idea!
      control2 = control
      control2$infill.crit = crit.mr
      control2$propose.points = 1L
      prop = proposePointsByInfillOptimization(opt.state, control = control2)
      list(
        best.ind = NA_integer_,
        x = prop$prop.points[1,],
        y = prop$crit.vals[1]
      )
    } else {
      stop ("should not happen")
    }
  } else {
    inds = getOptPathParetoFront(opt.path, index = TRUE)
    pareto.set = lapply(inds, function(i) getOptPathEl(opt.path, i)$x)
    if (unify) {
      list(
        x = do.call(rbind.data.frame ,pareto.set),
        y = getOptPathParetoFront(opt.path),
        best.ind = inds
      )
    } else {
      list(
        pareto.front = getOptPathY(opt.path)[inds, , drop = FALSE],
        pareto.set = pareto.set,
        inds = inds
      )
    }
  }
}

getOptStateState = function(opt.state) {
  opt.state$state
}

getOptStateTermination = function(opt.state) {
  terminate = shouldTerminate.OptState(opt.state)
  # update only if termination condition is met
  if (terminate$term) {
    setOptStateState(opt.state, terminate$code)
  }
  terminate
}

getOptStateValidStates = function() {
  c("init", "iter", getOptStateValidTerminationStates())
}

getOptStateValidTerminationStates = function() {
  c("term.iter", "term.time", "term.exectime", "term.yval", "term.feval", "term.custom")
}

# concept drift additions #####

getOptStateFixedParam = function(opt.state) {
  dob = getOptStateLoop(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  if (!is.null(getOptProblemDriftParam(opt.problem)) && !control$conceptdrift.learn.drift) {
    res = list(control$conceptdrift.drift.function(dob))
    res = setNames(res, getOptProblemDriftParam(opt.problem))
  } else {
    res = list()
  }
  res
}

getOptStateFixedLearnableParam = function(opt.state) {
  dob = getOptStateLoop(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  if (!is.null(getOptProblemDriftParam(opt.problem)) && control$conceptdrift.learn.drift) {
    res = list(control$conceptdrift.drift.function(dob))
    res = setNames(res, getOptProblemDriftParam(opt.problem))
  } else {
    res = list()
  }
  res
}

getOptStateParSet = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  if (!is.null(getOptProblemDriftParam(opt.problem)) && control$conceptdrift.learn.drift) {
    par.set = getOptProblemParSet(opt.problem, original.par.set = TRUE)
    fixed.x = getOptStateFixedLearnableParam(opt.state)
    fixParamSet(par.set, fixed.x)
  } else {
    getOptProblemParSet(opt.problem)
  }
}
