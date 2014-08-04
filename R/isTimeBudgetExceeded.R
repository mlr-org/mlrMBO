# Helper which checks if time budget is exceeded.
# 
# Returns logical value.
#
# @param start.time [\code{POSIXct}]
#   Starting time of mbo agorithm (result of Sys.time() call).
# @param time.budget [\code{integer(1)}]\cr
#   Time budget in seconds.
# @return [\code{logical(1)}]
isTimeBudgetExceeded = function(start.time, time.budget) {
  if (is.null(time.budget) || is.infinite(time.budget))
    return(FALSE)
  current.time = Sys.time()
  time.difference = as.numeric(difftime(current.time, start.time, units = "secs"))
  return(time.difference > time.budget)
}