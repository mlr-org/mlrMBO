#' @title OptState object.
#' @description
#' The OptState is the central component of the mbo iterations.
#' This environment contains every necessary information needed during optimization in MBO.
#' It also links to the \code{\link{OptProblem}} and to the \code{\link{OptResult}}.
#' @name OptState
#' @rdname OptState
NULL

# @param loop \code{integer()} \cr
#   Tells us in what loop we are at the moment. 0 means we are in the initial phase.
#   The loop i should change to i+1 as soon as the i-th point is evaluated
# @param tasks \code{list()} \cr
#   List of \code{RegrTask} which together formulate data necessary for the surrogate.
#   Caching is done to not necessarily regenerate tasks if the \code{loop} has not changed yes
#  @param models \code{list()} \cr
#    List of \code{WrappedModel} which are trained on the \code{tasks} and formulate the surrogate.
#    Caching is done as above.
#  @param time.model \code{WrappedModel} \cr
#    Models the evaluation time given the function values.
#  @param opt.result \code{OptResult} \cr
#    Pointer to the OptResult Object.
#  @param state \code{character(1)} \cr
#    Tells us in what state we are in text. So far we know:
#    init - right after initialization of mbo
#    iter - within an iteration
#    term.iter - maximal number of iterations reached
#    term.time - maximal running time exceeded
#    term.exectime - maximal execution time reached
#    term.yval - target fun value reached
#    term.fevals - maximal number of function evaluations reached
#    term.custom - terminated due to custom termination condition
#  @param opt.path \code{OptPath} \cr
#    Here we keep the opt.path. It delivers the data for the tasks and other useful information.
#  @param time.last.saved \code{POSIXct} \cr
#    The \code{Sys.time()} when the last save on disk was done.
#  @param loop.starttime \code{POSIXct} \cr
#    The \code{Sys.time()} when the mbo iteration was started.
#  @param time.used \code{integer(1)} \cr
#    The time in seconds we are already used for optimization since the very start.
#    This counts all iterations together and is necessary for continuation with a given time budget.

# IMPORTANT NOTE:
# See this as a constructor and it's variables as member variables.
# All variables in this Object should be documented here.
# Think of it, when you implement new ones!
# Unfortunately in R we cannot hinder you from putting other values in this object, but please: Don't!

makeOptState = function(opt.problem, loop = 0L, tasks = NULL, models = NULL,
  time.model = NULL, opt.result = NULL, state = "init", opt.path = NULL,
  time.last.saved = Sys.time(), loop.starttime = Sys.time(), time.used = 0L, time.created = Sys.time()) {

  opt.state = new.env()

  opt.state$opt.problem = opt.problem
  opt.state$loop = loop #the loop the state is IN, not the one it is finished
  opt.state$tasks = tasks
  opt.state$models = models
  opt.state$models.loop = -1L #the loop the models where generated
  opt.state$tasks.loop = -1L #the loop the tasks where generated
  opt.state$time.model = time.model
  opt.state$opt.result = coalesce(opt.result, makeOptResult())
  opt.state$state = state #possible states: init, iter, iter.exceeded, time.exceeded, exec.time.exceeded
  opt.state$opt.path = coalesce(opt.path, makeMBOOptPath(opt.problem))
  opt.state$time.last.saved = time.last.saved
  opt.state$loop.starttime = loop.starttime
  opt.state$time.used = time.used

  opt.state$random.seed = getRandomSeed()
  opt.state$time.created = time.created
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
  set.seed(getOptStateRandomSeed(opt.state))
  return(opt.state)
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

print.OptState = function(x, ...) {
  catf("OptSate")
  catf("Actual state: %s", getOptStateState(x))
  catf("Actual loop: %i", getOptStateLoop(x))
  catf("Loop started:%s", x$loop.starttime)
  catf("")
  print(getOptStateOptProblem(x))
  catf("")
  print(getOptStateOptPath(x))
}
