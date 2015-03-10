# Helper which checks if time budget is exceeded.
#
# Returns logical value.
#
# @param x [\code{POSIXct} | \code{OptPath}]
#   Starting time of mbo agorithm (result of Sys.time() call) or opt.path.
# @param time.budget [\code{integer(1)}]\cr
#   Time budget in seconds.
# @return [\code{logical(1)}]

isTimeBudgetExceeded = function(x, time.budget) {
	UseMethod("isTimeBudgetExceeded")
}

isTimeBudgetExceeded.POSIXct = function(x, time.budget) {
  if (is.null(time.budget) || is.infinite(time.budget))
    return(FALSE)
  current.time = Sys.time()
  time.difference = as.numeric(difftime(current.time, x, units = "secs"))
  return(time.difference > time.budget)
}

isTimeBudgetExceeded.OptPath = function(x, time.budget) {
  if (is.null(time.budget) || is.infinite(time.budget))
    return(FALSE)
  return(sum(getOptPathExecTimes(x)) > time.budget)
}


