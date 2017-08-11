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

getOptStateTasks = function(opt.state) {
  if (is.null(opt.state$tasks) || getOptStateLoop(opt.state) != opt.state$tasks.loop) {
    tasks = makeTasks(opt.state)
    setOptStateTasks(opt.state, tasks)
  } else {
    tasks = opt.state$tasks
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
  opt.state$opt.path
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
    final.index = chooseFinalPoint(opt.state)
    list(
      x = getOptPathX(opt.path)[final.index, , drop = FALSE],
      y = getOptPathY(opt.path)[final.index],
      best.ind = final.index
    )
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
