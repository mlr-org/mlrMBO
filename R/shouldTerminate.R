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
#   maximum number of iterations is reached and 1 if time budget is exceeded.
shouldTerminate = function(max.iters, iter, time.budget, start.time, exec.time.budget, opt.path = NULL, show.info = FALSE) {
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
  return(-1L)
}
