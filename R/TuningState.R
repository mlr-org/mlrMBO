## TuningState is the central component of the mbo iterations. This enviroment contains every necessary information we need during tuning in MBO. It links to the \code{TuningProblem} and to the \code{TuningResult}.
## @param loop \code{integer()} \cr
##   Tells us in what loop we are at the moment. 0 means we are in the inital phase.
##   The loop i should change to i+1 as soon as the i-th point is evaluated
## @param tasks \code{list()} \cr
##   List of \code{RegrTask} which together formulate data neccessary for the surrogate.
##   Caching is done to not neccessarly regenerate tasks if the \code{loop} has not changed yes
##  @param models \code{list()} \cr
##    List of \code{WrappedModel} which are trained on the \code{tasks} and formulate the surrogate. 
##    Caching is done as above.
##  @param tuningResult \code{TuningResult} \cr
##    Pointer to the TuningResult Object.
##  @param state \code{character(1)} \cr
##    Tells us in what state we are in text. So far we know: init, iter, iter.exceeded, time.exceeded and exec.time.exceeded.
##  @param opt.path \code{OptPath} \cr
##    Here we keep the opt.path. It delivers the data for the tasks and other usefull information.
##  @param last.saved \code{POSIXct} \cr
##     The \code{Sys.time()} when the last save on disk was done.
makeTuningState = function(tuningProblem, loop = 0L, tasks = NULL, models = NULL, tuningResult = NULL, state = "init", opt.path = NULL, last.saved = Sys.time()) {

  tuningState = new.env()

  tuningState$tuningProblem = tuningProblem
  tuningState$loop = loop #the loop the state is IN, not the one it is finished
  tuningState$tasks = tasks
  tuningState$models = models
  tuningState$models.loop = -1
  tuningState$tasks.loop = -1

  if (is.null(tuningResult)) {
    tuningState$tuningResult = makeTuningResult()
  } else {
    tuningState$tuningResult = tuningResult
  }
  
  tuningState$state = state #states = init, iter, iter.exceeded, time.exceeded, exec.time.exceeded, 

  if (is.null(opt.path)) {
    tuningState$opt.path = makeMBOOptPath(
      getTuningProblemParSet(tuningProblem),
      getTuningProblemControl(tuningProblem)
    )  
  } else {
    tuningState$opt.path = opt.path
  }
  tuningState$last.saved = last.saved

  tuningState$random.seed = .Random.seed
  class(tuningState) = append(class(tuningState), "TuningState")
  tuningState
}

getTuningStateTuningProblem = function(tuningState) {
  tuningState$tuningProblem
}

getTuningStateModels = function(tuningState) {
  if (getTuningStateLoop(tuningState) != tuningState$models.loop) {
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
  tuningState$models.loop = getTuningStateLoop(tuningState)
  invisible()
}

setTuningStateRandomSeed = function(tuningState) {
  tuningState$random.seed = .Random.seed
  invisible()
}

getTuningStateRandomSeed = function(tuningState) {
  tuningState$random.seed
}


getTuningStateTasks = function(tuningState) {
	if (getTuningStateLoop(tuningState) != tuningState$tasks.loop) {
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
  tuningState$tasks.loop = getTuningStateLoop(tuningState)
  invisible()
}

getTuningStateTuningResult = function(tuningState) {
  tuningState$tuningResult
}

setTuningStateLoop = function(tuningState, loop = NULL) {
  tuningResult = getTuningStateTuningResult(tuningState)
  setTuningResultResampleResults(tuningResult, tuningState)
  setTuningResultStoredModels(tuningResult, tuningState)
  if (is.null(loop))
    tuningState$loop = tuningState$loop + 1
  else
    tuningState$loop = loop
      # save resampling and models in result routine
  setTuningStateRandomSeed(tuningState)
  invisible()
}

getTuningStateLoop = function(tuningState) {
  tuningState$loop
}

getTuningStateOptPath = function(tuningState) {
  tuningState$opt.path
}

setTuningStateLastSaved = function(tuningState, time) {
  tuningState$last.saved = time
  invisible()
}

getTuningStateLastSaved = function(tuningState) {
  tuningState$last.saved
}

getTuningStateShouldSave = function(tuningState) {
  tuningProblem = getTuningStateTuningProblem(tuningState)
  control = getTuningProblemControl(tuningProblem)

  getTuningStateLoop(tuningState) %in% control$save.on.disk.at ||
    difftime(Sys.time(), getTuningStateLastSaved(tuningState), units = "secs") > control$save.on.disk.at.time
  
}

saveTuningState = function(tuningState) {
  loop = getTuningStateLoop(tuningState)
  control = getTuningProblemControl(getTuningStateTuningProblem(tuningState))
  show.info = getTuningProblemShowInfo(getTuningStateTuningProblem(tuningState))
  fn = control$save.file.path
  backup.fn = getFileBackupName(fn)
  save2(file = backup.fn, tuningState = tuningState)
  file.copy(backup.fn, fn, overwrite = TRUE)
  file.remove(backup.fn)
  setTuningStateLastSaved(tuningState, Sys.time())
  if (loop <= control$iters)
    showInfo(show.info, "Saved the current state after iteration %i in the file %s.",
      loop, control$save.file.path)
  else
    showInfo(show.info, "Saved the final state in the file %s", control$save.file.path)
}

loadTuningState = function(obj) {
  UseMethod("loadTuningState")
}

loadTuningState.TuningProblem = function(obj) {
  fn = getTuningProblemControl(obj)$save.file.path
  loadTuningState(fn)
}

loadTuningState.character = function(obj) {
  tuningState = load2(file = obj, "tuningState")
  .Random.seed = getTuningStateRandomSeed(tuningState)
  tuningState
}


# @param unify [\code{logical(1)}]
#   Defines if in the case of multicriterial optimization we shoud try to make the output similar to the result of the normal optimization.
getTuningStateFinalPoints = function(tuningState, unify = FALSE) {
  tuningProblem = getTuningStateTuningProblem(tuningState)
  control = getTuningProblemControl(tuningProblem)
  opt.path = getTuningStateOptPath(tuningState)

  if (control$number.of.targets == 1L) {
    final.index = chooseFinalPoint(tuningState)
    best = getOptPathEl(opt.path, final.index)
    list(
      x = dropNamed(best$x, ".multifid.lvl"),
      y = as.numeric(best$y),
      best.ind = final.index)
  } else {
    inds = getOptPathParetoFront(opt.path, index = TRUE)
    pareto.set = lapply(inds, function(i) getOptPathEl(opt.path, i)$x)
    if (unify) {
      list(
        x = do.call(rbind.data.frame ,pareto.set),
        y = getOptPathParetoFront(opt.path),
        best.ind = inds)  
    } else {
      list(
        pareto.front = getOptPathY(opt.path)[inds, , drop = FALSE],
        pareto.set = pareto.set,
        inds = inds
        )
    }   
  }
}

setTuningStateState = function(tuningState, state) {
  assertSubset(state, c("init", "iter", "iter.exceeded", "time.exceeded", "exec.time.exceeded"))
  tuningState$state = state
  invisible()
}

getTuningStateState = function(tuningState) {
  tuningState$state
}

getTuningStateTermination = function(tuningState) {
  terminate = shouldTerminate.TuningState(tuningState)
  if (terminate == 0) {
    setTuningStateState(tuningState, "iter.exceeded")
  } else if (terminate == 1) {
    setTuningStateState(tuningState, "time.exceeded")
  } else if (terminate == 2) {
    setTuningStateState(tuningState, "exec.time.exceeded")
  } else if (terminate == -1) {
    setTuningStateState(tuningState, "iter")
  } else {
    stopf("shouldTerminate() gave unexpected result: %i", terminate)
  }
  terminate
}
