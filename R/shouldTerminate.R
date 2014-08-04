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
# @return [\code{logical(1)}] Should the mbo algorithm terminate?
shouldTerminate = function(max.iters, iter, time.budget, start.time) {
  if (is.null(max.iters))
    max.iters = Inf
  if (is.null(time.budget))
    time.budget = Inf
  return(iter > max.iters | isTimeBudgetExceeded(start.time, time.budget))
}