# Helper which checks stopping criteria.
#
# @param max.iters [\code{integer(1)} | NULL]\cr
#   Maximal number of iterations.
# @param iter [\code{integer(1)}]\cr
#   Current iteration.
# @param time.budget [\code{integer(1)}]\cr
#   Time budget in seconds.
# @param start.time [\code{POSIXct}]
#   Starting time of mbo agorithm (result of Sys.time() call).
# @param exec.time.budget [\code{integer(1)}]\cr
#   Time budget in seconds for execution (read from opt.path).
# @param opt.path [\code{OptPath}]\cr
# @return [\code{integer(1)}] Negative value is no stopping criterion is satisfied. 0 if
#   maximum number of iterations is reached, 1 if time budget is exceeded and 3 if
#   targete function value is reached
## FIXME: documentation for 2 is missing
shouldTerminate = function(max.iters, iter, time.budget, start.time, exec.time.budget,
  target.fun.value, minimize, opt.path = NULL, show.info = FALSE) {
  if (iter > max.iters){
    showInfo(show.info, "max.iters %i reached with %i", max.iters, iter)
  	return(0L)
  }
  if (isTimeBudgetExceeded(start.time, time.budget)) {
  	return(1L)
  }
  if (isTimeBudgetExceeded(opt.path, exec.time.budget)) {
  	return(2L)
  }
  opt.dir = if (minimize) 1L else -1L
  current.best =  getOptPathEl(opt.path, getOptPathBestIndex((opt.path)))$y
  if (current.best * opt.dir < target.fun.value * opt.dir) {
    return(3L)
  }
  return(-1L)
}

shouldTerminate.OptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  shouldTerminate(
    max.iters = control$iters,
    iter = getOptStateLoop(opt.state),
    time.budget = control$time.budget,
    start.time = getOptProblemStartTime(opt.problem),
    exec.time.budget = control$exec.time.budget,
    target.fun.value = control$target.fun.value,
    minimize = control$minimize,
    opt.path = getOptStateOptPath(opt.state),
    show.info = getOptProblemShowInfo(opt.problem)
  )

}
