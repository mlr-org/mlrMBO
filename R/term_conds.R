# Termination conditions.
# Used to evantually stop mbo optimization process

# Each termination condition is a simple function which expects an opt.state
# and returns a list with the following three components
# * term: logical indicating whether the termination condition is met.
# * message: String indicating the reason for termination.
# * code: short character code of termination condition (only for build-in termination conditions!)
#   Possible value are term.exectime, term.time, term.feval, term.yval. Besides,
#   term.custom is assigned internally for custom, user-made termination conditions.

# @title
# Maximum iteration termination condition.
#
# @param max.iter [integer(1)]
#   Maximum number of iterations.
makeMBOTerminationMaxIter = function(max.iter) {
  assertCount(max.iter, na.ok = FALSE, positive = TRUE)
  force(max.iter)
  function(opt.state) {
    iter = getOptStateLoop(opt.state)
    term = iter > max.iter
    message = if (!term) NA_character_ else sprintf("Maximum number of iterations %i reached with.", max.iter, iter)
    return(list(term = term, message = message, code = "term.iter", progress = iter / max.iter))
  }
}

# @title
# Time budget termination condition.
#
# @param time.budget [numeric(1)]
#   Time budget in seconds.
makeMBOTerminationMaxBudget = function(time.budget) {
  assertNumber(time.budget, na.ok = FALSE)
  force(time.budget)
  function(opt.state) {
    time.used = as.numeric(getOptStateTimeUsed(opt.state), units = "secs")
    term = (time.used > time.budget)
    message = if (!term) NA_character_ else sprintf("Time budged %f reached.", time.budget)
    return(list(term = term, message = message, code = "term.time", progress = time.used / time.budget))
  }
}

# @title
# Execution time budget termination condition.
#
# @param time.budget [numeric(1)]
#   Exceution time budget in seconds.
makeMBOTerminationMaxExecBudget = function(time.budget) {
  assertNumber(time.budget, na.ok = FALSE)
  force(time.budget)
  function(opt.state) {
    opt.path = getOptStateOptPath(opt.state)
    exec.times = getOptPathExecTimes(opt.path)
    time.used = sum(exec.times, na.rm = TRUE)
    time.used.optimization = sum(exec.times[getOptPathDOB(opt.path)!=0], na.rm = TRUE)
    time.used.init = sum(exec.times[getOptPathDOB(opt.path)==0], na.rm = TRUE)
    term = (time.used > time.budget)
    message = if (!term) NA_character_ else sprintf("Execution time budged %f reached.", time.budget)
    return(list(term = term, message = message, code = "term.exectime", progress = max(time.used.optimization / (time.budget - time.used.init), 0, na.rm = TRUE)))
  }
}

# @title
# y-value termination condition.
#
# @param time.budget [numeric(1)]
#   Traget function value.
#
# @note: only for single-criteria functions.
makeMBOTerminationTargetFunValue = function(target.fun.value) {
  assertNumber(target.fun.value, na.ok = FALSE)
  force(target.fun.value)
  function(opt.state) {
    opt.problem = getOptStateOptProblem(opt.state)
    control = getOptProblemControl(opt.problem)
    opt.path = getOptStateOptPath(opt.state)
    opt.dir = if (control$minimize) 1L else -1L
    init.best = getOptPathY(opt.path)[getOptPathBestIndex(opt.path, dob = 0)]
    current.best = getOptPathY(opt.path)[getOptPathBestIndex(opt.path)]
    term = (current.best * opt.dir <= target.fun.value * opt.dir)
    message = if (!term) NA_character_ else sprintf("Target function value %f reached.", target.fun.value)
    return(list(term = term, message = message, code = "term.yval", progress = abs(init.best - current.best)/abs(init.best - target.fun.value)))
  }
}

# @title
# Maximal function evaluations termination condition.
#
# @param max.evals [integer(1)]
#   Maximal number of function evaluations.
makeMBOTerminationMaxEvals = function(max.evals) {
  max.evals = asInt(max.evals, na.ok = FALSE, lower = 1L)
  force(max.evals)
  function(opt.state) {
    opt.path = getOptStateOptPath(opt.state)
    evals = getOptPathLength(opt.path)
    init.evals = sum(getOptPathDOB(opt.path)==0)
    term = (evals >= max.evals)
    message = if (!term) NA_character_ else sprintf("Maximal number of function evaluations %i reached.", max.evals)
    return(list(term = term, message = message, code = "term.feval", progress = (evals-init.evals) / (max.evals-init.evals)))
  }
}
