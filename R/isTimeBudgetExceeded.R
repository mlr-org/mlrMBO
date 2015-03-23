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
  if (time.difference > time.budget) {
  	messagef("time.budget %i reached with %.1f", time.budget, time.difference)
  	return(TRUE)
  } else {
  	return(FALSE)
  }
}

isTimeBudgetExceeded.OptPath = function(x, time.budget) {
  if (is.null(time.budget) || is.infinite(time.budget))
    return(FALSE)
  exec.time = sum(getOptPathExecTimes(x))
  if (exec.time > time.budget) {
  	messagef("exec.time.budget %i reached with %.1f", time.budget, exec.time)
  	return(TRUE)
  } else {
  	return(FALSE)
  } 
}


