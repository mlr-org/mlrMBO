# Helper which checks termination criteria.
#
# @param opt.state [\code{OptState}]\cr
shouldTerminate.OptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  stop.conds = control$stop.conds

  for (stop.cond in stop.conds) {
    stop.obj = stop.cond(opt.state)
    if (stop.obj$term) {
      # if user-defined termination condition is active, set the code by hand
      if (is.null(stop.obj$code)) {
        stop.obj$code = "term.custom"
      }
      return(stop.obj)
    }
  }

  # "fallback"
  return(list(term = FALSE, message = NA_character_, code = NA_character_))
}
