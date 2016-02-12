# Helper which checks if time budget is exceeded.
#
# Returns logical value.
#
# @param x [\code{POSIXct} | \code{OptPath}]
#   Starting time of mbo agorithm (result of Sys.time() call) or opt.path.
# @param time.budget [\code{integer(1)}]\cr
#   Time budget in seconds.
# @return [\code{logical(1)}]

isTimeBudgetExceeded = function(x, time.budget, show.info = FALSE) {
	UseMethod("isTimeBudgetExceeded")
}

isTimeBudgetExceeded.OptPath = function(x, time.budget, show.info = FALSE) {
  if (is.null(time.budget) || is.infinite(time.budget))
    return(FALSE)
  exec.time = sum(getOptPathExecTimes(x))
  if (exec.time > time.budget) {
    showInfo(show.info, "exec.time.budget %i reached with %.1f", time.budget, exec.time)
  	return(TRUE)
  } else {
  	return(FALSE)
  }
}

isTimeBudgetExceeded.difftime = function(x, time.budget, show.info = FALSE) {
  x = as.numeric(x, units = "secs")
  isTimeBudgetExceeded(x, time.budget = time.budget, show.info = show.info)
}

isTimeBudgetExceeded.numeric = function(x, time.budget, show.info = FALSE) {
  if (is.null(time.budget) || is.infinite(time.budget))
    return(FALSE)
  if (x > time.budget) {
    showInfo(show.info, "time.budget %i reached with %.1f", time.budget, x)
    return(TRUE)
  } else {
    return(FALSE)
  }
}
