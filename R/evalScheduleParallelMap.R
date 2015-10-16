# Evaluates the target function using parallelMap

# @param wrapFun [\code{function}] \cr
#   A function which evaluates one item of xs and returns a list with the \code{y}, \code{time} and \code{user.extras}
# @param xs [\code{list}] \cr
#   A list of the x values to evaluate by \code{wrapFun}
# @param xs.times [\code{numeric}] \cr
#   A vector of the same length as \code{xs} giving the estimated times for each evaluation of \code{x}.
# @param xs.priorities [\code{numeric}] \cr
#   A vevtor of the same length as \code{xs} giving the priorities for each item in \code{xs}
# @param opt.state [\code{OptState}]\cr
# @return [\code{list}] \cr
#   List containing the results of \code{wrapFun} for each item in xs


evalScheduleParallelMap = function(wrapFun, xs, xs.times = NULL, xs.priorities = NULL, extras = NULL, opt.state) {
  # return error objects if we impute
  imputeY = getOptProblemControl(
    getOptStateOptProblem(opt.state))$impute.y.fun
  
  funRes = parallelMap(wrapFun, xs, level = "mlrMBO.feval",
    impute.error = if (is.null(imputeY)) NULL else identity)

  list(funRes = funRes, xs = xs, extras = extras, dob = asInteger(getOptStateLoop(opt.state)))
}

evalScheduleSmartParallelMap = function(wrapFun, xs, xs.times = NULL, xs.priorities = NULL, extras = NULL, opt.state) {

  schedule.nodes = getOptProblemControl(
    getOptStateOptProblem(opt.state))$schedule.nodes
  #filter xs to smart scheduling rule (not for init.design)
  if (!is.null(xs.priorities)) {
    order.idx = order(xs.priorities)
    xs = xs[order.idx]
    xs.times = xs.times[order.idx]
    xs.priorities = xs.priorities[order.idx]
  }
  
  occupied.time = integer(length = schedule.nodes)
  t.max = xs.times[1L] * 1.1
  del.xs = integer()
  for (i in seq_along(xs.times)) {
    scheduled = FALSE
    for (j in seq_len(schedule.nodes)) {
      if (t.max - occupied.time[j] >= xs.times[i]) {
        occupied.time[j] = occupied.time[j] + xs.times[i]
        scheduled = TRUE
        break()
      }
    }
    if (!scheduled) {
      del.xs = c(del.xs, i)
    }
  }

  work.inds = setdiff(seq_along(xs),del.xs)
  xs = xs[work.inds]
  extras = extras[work.inds]

  evalScheduleParallelMap(wrapFun = wrapFun, xs = xs, xs.times = xs.times, xs.priorities = xs.priorities, extras = extras, opt.state = opt.state)

}