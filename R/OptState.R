# OptState is the central component of the mbo iterations. This enviroment contains every necessary information we need during tuning in MBO. It links to the \code{OptProblem} and to the \code{OptResult}.
# @param loop \code{integer()} \cr
#   Tells us in what loop we are at the moment. 0 means we are in the inital phase.
#   The loop i should change to i+1 as soon as the i-th point is evaluated
# @param tasks \code{list()} \cr
#   List of \code{RegrTask} which together formulate data neccessary for the surrogate.
#   Caching is done to not neccessarly regenerate tasks if the \code{loop} has not changed yes
#  @param models \code{list()} \cr
#    List of \code{WrappedModel} which are trained on the \code{tasks} and formulate the surrogate. 
#    Caching is done as above.
#  @param opt.result \code{OptResult} \cr
#    Pointer to the OptResult Object.
#  @param state \code{character(1)} \cr
#    Tells us in what state we are in text. So far we know: init, iter, iter.exceeded, time.exceeded and exec.time.exceeded.
#  @param opt.path \code{OptPath} \cr
#    Here we keep the opt.path. It delivers the data for the tasks and other usefull information.
#  @param time.last.saved \code{POSIXct} \cr
#     The \code{Sys.time()} when the last save on disk was done.
makeOptState = function(opt.problem, loop = 0L, tasks = NULL, models = NULL, opt.result = NULL, state = "init", opt.path = NULL, time.last.saved = Sys.time()) {

  opt.state = new.env()

  opt.state$opt.problem = opt.problem
  opt.state$loop = loop #the loop the state is IN, not the one it is finished
  opt.state$tasks = tasks
  opt.state$models = models
  opt.state$models.loop = -1
  opt.state$tasks.loop = -1

  if (is.null(opt.result)) {
    opt.state$opt.result = makeOptResult()
  } else {
    opt.state$opt.result = opt.result
  }
  
  opt.state$state = state #states = init, iter, iter.exceeded, time.exceeded, exec.time.exceeded, 

  if (is.null(opt.path)) {
    opt.state$opt.path = makeMBOOptPath(
      getOptProblemParSet(opt.problem),
      getOptProblemControl(opt.problem)
    )  
  } else {
    opt.state$opt.path = opt.path
  }
  opt.state$time.last.saved = time.last.saved

  opt.state$random.seed = .Random.seed
  opt.state$time.created = Sys.time()
  class(opt.state) = append(class(opt.state), "OptState")
  opt.state
}

getOptStateOptProblem = function(opt.state) {
  opt.state$opt.problem
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

setOptStateModels = function(opt.state, models) {
  opt.state$models = models
  opt.state$models.loop = getOptStateLoop(opt.state)
  invisible()
}

setOptStateRandomSeed = function(opt.state) {
  opt.state$random.seed = .Random.seed
  invisible()
}

getOptStateRandomSeed = function(opt.state) {
  opt.state$random.seed
}


getOptStateTasks = function(opt.state) {
	if (is.null(opt.state$tasks) || getOptStateLoop(opt.state) != opt.state$tasks.loop) {
    opt.problem = getOptStateOptProblem(opt.state)
    tasks = makeTasks(
      opt.path = getOptStateOptPath(opt.state),
      algo.init = getOptProblemAlgoInit(opt.problem),
      control = getOptProblemControl(opt.problem))
    setOptStateTasks(opt.state, tasks)
	} else {
    tasks = opt.state$tasks
  }
  tasks
}

setOptStateTasks = function(opt.state, tasks) {
  opt.state$tasks = tasks
  opt.state$tasks.loop = getOptStateLoop(opt.state)
  invisible()
}

getOptStateOptResult = function(opt.state) {
  opt.state$opt.result
}

setOptStateLoop = function(opt.state, loop = NULL) {
  opt.result = getOptStateOptResult(opt.state)
  setOptResultResampleResults(opt.result, opt.state)
  setOptResultStoredModels(opt.result, opt.state)
  if (is.null(loop))
    opt.state$loop = opt.state$loop + 1
  else
    opt.state$loop = loop
      # save resampling and models in result routine
  setOptStateRandomSeed(opt.state)
  invisible()
}

getOptStateLoop = function(opt.state) {
  opt.state$loop
}

getOptStateOptPath = function(opt.state) {
  opt.state$opt.path
}

setOptStateTimeLastSaved = function(opt.state, time) {
  opt.state$time.last.saved = time
  invisible()
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

saveOptState = function(opt.state) {
  loop = getOptStateLoop(opt.state)
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  show.info = getOptProblemShowInfo(getOptStateOptProblem(opt.state))
  fn = control$save.file.path
  backup.fn = getFileBackupName(fn)
  save2(file = backup.fn, opt.state = opt.state)
  file.copy(backup.fn, fn, overwrite = TRUE)
  file.remove(backup.fn)
  setOptStateTimeLastSaved(opt.state, Sys.time())
  if (loop <= control$iters)
    showInfo(show.info, "Saved the current state after iteration %i in the file %s.",
      loop, control$save.file.path)
  else
    showInfo(show.info, "Saved the final state in the file %s", control$save.file.path)
}

loadOptState = function(obj) {
  UseMethod("loadOptState")
}

loadOptState.OptProblem = function(obj) {
  fn = getOptProblemControl(obj)$save.file.path
  loadOptState(fn)
}

loadOptState.character = function(obj) {
  opt.state = load2(file = obj, "opt.state")
  .Random.seed = getOptStateRandomSeed(opt.state)
  opt.state
}


# @param unify [\code{logical(1)}]
#   Defines if in the case of multicriterial optimization we shoud try to make the output similar to the result of the normal optimization.
getOptStateFinalPoints = function(opt.state, unify = FALSE) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)

  if (control$number.of.targets == 1L) {
    final.index = chooseFinalPoint(opt.state)
    best = getOptPathEl(opt.path, final.index)
    list(
      x = best$x,
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

setOptStateState = function(opt.state, state) {
  assertSubset(state, c("init", "iter", "iter.exceeded", "time.exceeded", "exec.time.exceeded"))
  opt.state$state = state
  invisible()
}

getOptStateState = function(opt.state) {
  opt.state$state
}

getOptStateTermination = function(opt.state) {
  terminate = shouldTerminate.OptState(opt.state)
  if (terminate == 0) {
    setOptStateState(opt.state, "iter.exceeded")
  } else if (terminate == 1) {
    setOptStateState(opt.state, "time.exceeded")
  } else if (terminate == 2) {
    setOptStateState(opt.state, "exec.time.exceeded")
  } else if (terminate == -1) {
    setOptStateState(opt.state, "iter")
  } else {
    stopf("shouldTerminate() gave unexpected result: %i", terminate)
  }
  terminate
}

getOptStateMboResult = function(opt.state) {
  # save final model if demanded
  setOptResultStoredModels(getOptStateOptResult(opt.state), opt.state)

  mbo.result = makeMBOResult.OptState(opt.state)
  setOptResultMboResult(getOptStateOptResult(opt.state), mbo.result)
  mbo.result
}