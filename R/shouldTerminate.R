# Helper which checks stopping criteria.
#
# @param opt.state [\code{OptState}]\cr
shouldTerminate.OptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  stop.conds = control$stop.conds

  for (stop.cond in stop.conds) {
    stop.obj = stop.cond(opt.state)
    if (stop.obj$term) {
      return(stop.obj)
    }
  }

  # "fallback"
  return(list(term = FALSE, message = NA_character_, code = 0L))
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
