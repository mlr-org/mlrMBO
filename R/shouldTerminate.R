# Helper which checks stopping criteria.
#
# @param opt.state [\code{OptState}]\cr

shouldTerminate.OptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  iter = getOptStateLoop(opt.state)
  start.time = getOptProblemStartTime(opt.problem)
  target.fun.value = control$target.fun.value
  minimize = control$minimize
  opt.path = getOptStateOptPath(opt.state)

  if (iter > control$iters){
    showInfo(getOptProblemShowInfo(opt.problem), "max.iters %i reached with %i", control$iters, iter)
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
<<<<<<< HEAD
    if (current.best * opt.dir <= target.fun.value * opt.dir) {
      return(3L)
=======
    if (current.best * opt.dir < target.fun.value * opt.dir) {
      return(4L)
>>>>>>> 3dd4007ec05664b9bfb36b498080a24d1dc3e634
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
