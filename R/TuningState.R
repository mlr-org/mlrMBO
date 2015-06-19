makeTuningState = function(tuningProblem, loop = 0L, tasks = NULL, models = NULL, tuningResult = makeTuningResult()) {

  tuningState = new.env()

  tuningState$tuningProblem = tuningProblem
  tuningState$loop = loop
  tuningState$tasks = tasks
  tuningState$models = models
  tuningState$tuningResult = tuningResult

  tuningState$opt.path = makeMBOOptPath(par.set, control)

  if (loop == 0L) {
    generateMBODesign.TuningState(
      tuningState = tuningState,
      tuningProblem = tuningProblem
    )
  }

  tuningState$random.seed = .Random.seed

  class(tuningState) = append(class(tuningState), "TuningState")
  tuningState
}

getTuningStateTuningProblem = function(tuningState) {
  tuningState$tuningProblem
}

getTuningStateModels = function(tuningState) {
  if (is.null(tuningState$models)) {
    tuningProblem = getTuningStateTuningProblem(tuningState)
  	models = trainModels(
  	  learner = getTuningProblemLearner(tuningProblem), 
  	  tasks = getTuningStateTasks(tuningState),
  	  control = getTuningProblemControl(tuningProblem))
  	setTuningStateModels(tuningState, models)
  } else {
  	models = tuningState$models
  }
  models
}

setTuningStateModels = function(tuningState, models) {
  tuningState$models = models
  invisible()
}

setTuningStateRandomSeed = function(tuningState) {
  tuningState$random.seed = .Random.seed
  invisible()
}

getTuningStateTasks = function(tuningState) {
	if (is.null(tuningState$tasks)) {
    tuningProblem = getTuningStateTuningProblem(tuningState)
    tasks = makeTasks(
      par.set = getTuningProblemParSet(tuningProblem),
      opt.path = getTuningStateOptPath(tuningState),
      algo.init = getTuningProblemAlgoInit(tuningProblem),
      control = getTuningProblemControl(tuningProblem))
    setTuningStateTasks(tuningState, tasks)
	} else {
    tasks = tuningState$tasks
  }
  tasks
}

setTuningStateTasks = function(tuningState, tasks) {
  tuningState$tasks = tasks
  invisible()
}

getTuningStateTuningResult = function(tuningState) {
  tuningState$tuningResult
}

setTuningStateLoop = function(tuningState, loop = NULL) {
  if (is.null(loop))
    tuningState$loop = tuningState$loop + 1
  else
    tuningState$loop = loop
  tuningResult = getTuningStateTuningResult(tuningState)
  setTuningResultResampleResults(tuningResult, tuningState)
  setTuningResultStoredModels(tuningResult, tuningState)
  setTuningStateRandomSeed(tuningState)
  saveTuningState(tuningState)
  tuningState$tasks = NULL
  tuningState$models = NULL
  invisible()
}

getTuningStateLoop = function(tuningState) {
  tuningState$loop
}

getTuningStateOptPath = function(tuningState) {
  tuningState$opt.path
}

saveTuningState = function(tuningState) {
  loop = getTuningStateLoop(tuningState)
  control = getTuningProblemControl(getTuningStateTuningProblem(tuningState))
  if (loop %in% control$save.on.disk.at) {
    saveTuningStateNow(tuningState)
  }
  invisible()
}

saveTuningStateNow = function(tuningState) {
  loop = getTuningStateLoop(tuningState)
  control = getTuningProblemControl(getTuningStateTuningProblem(tuningState))
  fn = control$save.file.path
  backup.fn = getFileBackupName(fn)
  save2(file = backup.fn, tuningState = tuningState)
  file.copy(backup.fn, fn, overwrite = TRUE)
  file.remove(backup.fn)
  if (loop <= control$iters)
    showInfo(show.info, "Saved the current state after iteration %i in the file %s.",
      loop, control$save.file.path)
  else
    showInfo(show.info, "Saved the final state in the file %s", control$save.file.path)
}

loadTuningState = function(tuningProblem) {
  fn = getTuningProblemControl(tuningProblem)$save.file.path
  load2(file = fn, "tuningState")
}

getTuningStateMBOResult = function(tuningState) {

}

getTuningStateFinalPoints = function(tuningState) {
  tuningProblem = getTuningStateTuningProblem(tuningState)
  control = getTuningProblemControl(tuningProblem)
  opt.path = getTuningStateOptPath(tuningState)

  if (control$number.of.targets == 1L) {
    final.index = chooseFinalPoint(
      fun = getTuningProblemFun(tuningProblem), 
      opt.path = opt.path, 
      model = getTuningStateModels(tuningState)$models[[1L]], 
      task = getTuningStateTasks(tuningState)[[1]], 
      control = control)
    best = getOptPathEl(opt.path, final.index)
  } else {
    inds = getOptPathParetoFront(opt.path, index = TRUE)
    pareto.set = lapply(inds, function(i) getOptPathEl(op, i)$x)
    list(
      x = do.call(rbind.data.frame ,pareto.set)
      y = getOptPathParetoFront(opt.path))
  }
}
