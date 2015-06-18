makeTuningState = function(tuningProblem, loop = 1L, tasks = NULL, models = NULL) {

  tuningState = new.env()

  tuningState$tuningProblem = tuningProblem
  tuningState$loop = loop
  tuningState$tasks = tasks
  tuningState$models = models

  tuningState$opt.path = makeMBOOptPath(par.set, control)

  if (loop = 1L) {
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

setTuningStateLoop = function(tuningState, loop = NULL) {
  if (is.null(loop))
    tuningState$loop = tuningState$loop + 1
  else
    tuningState$loop = loop
  setTuningStateRandomSeed(tuningState)
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
  invisible()
}

loadTuningState = function(tuningProblem) {
  fn = getTuningProblemControl(tuningProblem)$save.file.path
  load2(file = fn, "tuningState")
}