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
# @return [\code{integer(1)}] Negative value is no stopping criterion is satisfied. 0 if
#   maximum number of iterations is reached and 1 if time budget is exceeded.
shouldTerminate = function(max.iters, iter, time.budget, start.time) {
  if (iter > max.iters)
  	return(0L)
  if (isTimeBudgetExceeded(start.time, time.budget))
  	return(1L)
  return(-1L)
}