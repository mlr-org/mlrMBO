#' @title OptState object.
#' @description
#' The OptState is the central component of the mbo iterations. 
#' This enviroment contains every necessary information needed during optimization in MBO. 
#' It also links to the \code{\link{OptProblem}} and to the \code{\link{OptResult}}.
#' @name OptState
#' @rdname OptState
NULL

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
#    Tells us in what state we are in text. So far we know: init, iter,
#    iter.exceeded, time.exceeded, exec.time.exceeded, target.fun.value.reached, manual.exceeded
#  @param opt.path \code{OptPath} \cr
#    Here we keep the opt.path. It delivers the data for the tasks and other usefull information.
#  @param time.last.saved \code{POSIXct} \cr
#     The \code{Sys.time()} when the last save on disk was done.

makeOptState = function(opt.problem, loop = 0L, tasks = NULL, models = NULL, 
  time.model = NULL, opt.result = NULL, state = "init", opt.path = NULL, 
  time.last.saved = Sys.time()) {

  opt.state = new.env()

  opt.state$opt.problem = opt.problem
  opt.state$loop = loop #the loop the state is IN, not the one it is finished
  opt.state$tasks = tasks
  opt.state$models = models
  opt.state$models.loop = -1L
  opt.state$tasks.loop = -1L
  opt.state$time.model = time.model

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

saveOptState = function(opt.state, file = NULL) {
  loop = getOptStateLoop(opt.state)
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  show.info = getOptProblemShowInfo(getOptStateOptProblem(opt.state))
  if (is.null(file)) {
    fn = control$save.file.path  
  } else {
    fn = file
  }
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

# If we already have a mbo result we will return it, otherwise it will be generated and stored in the opt.result
makeOptStateMboResult = function(opt.state) {
  opt.result = getOptStateOptResult(opt.state)
  
  # save final model if demanded
  setOptResultStoredModels(opt.result, opt.state)
  mbo.result = makeMBOResult.OptState(opt.state)
  setOptResultMboResult(opt.result, mbo.result)
  mbo.result
}