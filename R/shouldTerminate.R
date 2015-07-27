# Helper which checks stopping criteria.
#
# @param opt.state [\code{OptState}]\cr
#   Maximal number of iterations.
# @param iter [\code{integer(1)}]\cr
#   Current iteration.
# @param time.budget [\code{integer(1)}]\cr
#   Time budget in seconds.
# @param start.time [\code{POSIXct}]
#   Starting time of mbo agorithm (result of Sys.time() call).
# @param exec.time.budget [\code{integer(1)}]\cr
#   Time budget in seconds for execution (read from opt.path).
# @param target.fun.value [\code{numeric(1)}]
#   Optimal function value to reach
# @param minimize [\code{logical}]
#   Minimize value from control
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
  if (isTimeBudgetExceeded(start.time, control$time.budget)) {
    return(2L)
  }
  if (isTimeBudgetExceeded(opt.path, control$exec.time.budget)) {
    return(3L)
  }
  # target.fun.value only useful for single crit
  if (length(minimize) == 1L) {
    opt.dir = if (minimize) 1L else -1L
    current.best =  getOptPathEl(opt.path, getOptPathBestIndex((opt.path)))$y
    if (current.best * opt.dir <= target.fun.value * opt.dir) {
      return(4L)
    }
  }
  return(0)
}

# This function returns all the character which lead to an termination 
# of the MBO Iteratio if x == NULL, otherwise the representative reason
# according to the number returned in shouldTerminate()
getTerminateChars = function(x = NULL) {
  final.states = c(iter = "iter.exceeded", time = "time.exceeded", 
    exec.time = "exec.time.exceeded", target = "target.fun.value.reached", 
    manual = "manual.exceeded")
  if (is.null(x)) {
    return(final.states)
  } else if (x == 0) {
    return("iter")
  } else {
    return(as.character(final.states[x]))
  } 
}