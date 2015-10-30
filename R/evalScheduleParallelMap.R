# Evaluates the target function using parallelMap

# @param wrapFun [\code{function}] \cr
#   A function which evaluates one item of xs and returns a list with the \code{y}, \code{time} and \code{user.extras}
# @param xs [\code{list}] \cr
#   A list of the x values to evaluate by \code{wrapFun}
# @param xs.schedule.info [\code{list}] \cr
#   A list containing vectors of the same length as \code{xs} giving the estimated times, priorities and times.se for each evaluation of \code{x}.
# @param opt.state [\code{OptState}]\cr
# @return [\code{list}] \cr
#   List containing the results of \code{wrapFun} for each item in xs


evalScheduleParallelMap = function(wrapFun, xs, xs.schedule.info = NULL, extras = NULL, opt.state) {
  # return error objects if we impute
  imputeY = getOptProblemControl(
    getOptStateOptProblem(opt.state))$impute.y.fun
  
  funRes = parallelMap(wrapFun, xs, level = "mlrMBO.feval",
    impute.error = if (is.null(imputeY)) NULL else identity)

  list(funRes = funRes, xs = xs, extras = extras, dob = asInteger(getOptStateLoop(opt.state)))
}

evalScheduleSmartParallelMap = function(wrapFun, xs, xs.schedule.info = NULL, extras = NULL, opt.state) {
  xs.times = xs.schedule.info$times
  if (!is.null(xs.times)) {
    schedule.nodes = getOptProblemControl(
      getOptStateOptProblem(opt.state))$schedule.nodes
    #filter xs to smart scheduling rule (not for init.design)
    if (!is.null(xs.schedule.info$priorities)) {
      order.idx = order(xs.schedule.info$priorities)
      xs = xs[order.idx]
      xs.schedule.info = lapply(xs.schedule.info, function(x) x[order.idx])
    }

    
    xs.times.se = xs.schedule.info$times.se
    
    occupied.time = integer(length = schedule.nodes)
    t.max = xs.times[1L] + 1.96 * xs.times.se[1L]
    scheduled.job = integer() #which job got scheduled
    scheduled.on = integer() #on which node is it scheduled
    scheduled.at = integer() #at what time is is scheduled
    for (i in seq_along(xs.times)) {
      for (j in seq_len(schedule.nodes)) {
        if (t.max - occupied.time[j] >= xs.times[i]) {
          scheduled.job = c(scheduled.job, i)
          scheduled.on = c(scheduled.on, j)
          scheduled.at = c(scheduled.at, occupied.time[j])
          occupied.time[j] = occupied.time[j] + xs.times[i]
          break()
        }
      }
    }

    xs = xs[scheduled.job]
    extras = extras[scheduled.job]
    for (i in seq_along(scheduled.job)) {
      extras[[i]]$scheduled.job = scheduled.job[i]
      extras[[i]]$scheduled.on = scheduled.on[i]
      extras[[i]]$scheduled.at = scheduled.at[i]  
    }
  }
  evalScheduleParallelMap(wrapFun = wrapFun, xs = xs, xs.schedule.info = xs.schedule.info, extras = extras, opt.state = opt.state)

}
