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


evalScheduleParallelMap = function(wrapFun, xs, xs.trafo, xs.schedule.info = NULL, extras = NULL, opt.state) {
  # return error objects if we impute
  imputeY = getOptProblemControl(
    getOptStateOptProblem(opt.state))$impute.y.fun
  
  funRes = parallelMap(wrapFun, xs.trafo, level = "mlrMBO.feval",
    impute.error = if (is.null(imputeY)) NULL else identity)

  list(funRes = funRes, xs = xs, xs.trafo = xs.trafo, extras = extras, dob = asInteger(getOptStateLoop(opt.state)))
}

evalScheduleSmartParallelMap = function(wrapFun, xs, xs.trafo, xs.schedule.info = NULL, extras = NULL, opt.state) {

  if (!is.null(xs.schedule.info$times)) {
    control = getOptProblemControl(getOptStateOptProblem(opt.state))
    schedule.nodes = control$schedule.nodes
    
    # order everything according to priorities in xs.schedule.info
    if (!is.null(xs.schedule.info$priorities)) {
      order.idx = order(xs.schedule.info$priorities, decreasing = TRUE)
      xs = xs[order.idx]
      xs.trafo = xs.trafo[order.idx]
      xs.schedule.info = xs.schedule.info[order.idx,, drop = FALSE]
      extras = extras[order.idx]
    }

    t.max = xs.schedule.info$times[1L] + 0.05 * xs.schedule.info$times[1L]
    
    # schedule where(on) which job(job) will be executed at which time(at)
    occupied.time = double(length = schedule.nodes)
    scheduled = data.frame(
      job = integer(), #which job got scheduled
      on = integer(), #on which node is it scheduled
      at = double() #at what time is is scheduled
      )
    for (i in seq_along(xs)) {
      for (j in seq_len(schedule.nodes)) {
        if (t.max - occupied.time[j] >= xs.schedule.info$times[i]) {
          scheduled = rbind(scheduled, list(job = i, on = j, at = occupied.time[j]))
          occupied.time[j] = occupied.time[j] + xs.schedule.info$times[i]
          break
        }
      }
    }

    # reorder jobs to suit the load balancer
    load.balance.order = order(scheduled$at, decreasing = FALSE)
    scheduled = scheduled[load.balance.order,]
    xs = xs[scheduled$job]
    xs.trafo = xs.trafo[scheduled$job]
    xs.schedule.info[scheduled$job,]
    extras = extras[scheduled$job]

    #fill empty nodes with random jobs below time threashold
    #FIXME dirty hack to fill stuff
    if (control$schedule.fill.random && (empty.slots = schedule.nodes - nrow(scheduled))>0) {
      par.set = getOptProblemParSet(getOptStateOptProblem(opt.state))
      prop = proposePointsRandom2(par.set = par.set, n = empty.slots * 50)
      control2 = control
      control2$infill.crit = "random"
      control2$propose.points = empty.slots * 50
      time.model = getOptStateTimeModel(opt.state)
      time.prediction = predict(time.model, newdata = prop$prop.points)
      predicted.time = getPredictionResponse(time.prediction)
      #partly taken from evalProposedPoints.OptState
      extras2 = getExtras(
        n = nrow(prop$prop.points),
        prop = c(prop, list(
          predicted.time = predicted.time,
          predicted.time.se = getPredictionSE(time.prediction))),
        train.time = NA_real_,
        control = control2)
      for (i in seq_along(extras2)) {
         #fixme not very safe
        names(extras2[[i]]) = names(extras[[1]])
      }
      xs2 = dfRowsToList(prop$prop.points, par.set)
      xs2 = lapply(xs2, repairPoint, par.set = par.set)
      #narrow down to fit inside of t.max
      inds = which(predicted.time < t.max)
      #narrow down to one job for each free node
      inds = head(inds, empty.slots)
      scheduled = rbind(scheduled, list(job = length(xs) + seq_along(inds), on = max(scheduled$on) + seq_along(inds), at = occupied.time[max(scheduled$on) + seq_along(inds)]))
      extras = c(extras, extras2[inds])
      xs = c(xs, xs2[inds])
      xs2.trafo = apply(xs2[inds], trafoValue, par = getOptProblemParSet(getOptStateOptProblem(opt.state)))
      xs.trafo = c(xs.trafo, xs2.trafo)
    }

    #put scheduling information into extras
    for (i in seq_row(scheduled)) {
      extras[[i]]$scheduled.job = scheduled$job[i]
      extras[[i]]$scheduled.on = scheduled$on[i]
      extras[[i]]$scheduled.at = scheduled$at[i]  
      extras[[i]]$scheduled.priority = xs.schedule.info$priorities[i]
    }
  }

  #FIXME: xs.schedule.info auch noch sortieren?
  evalScheduleParallelMap(wrapFun = wrapFun, xs = xs, xs.trafo = xs.trafo, xs.schedule.info = xs.schedule.info, extras = extras, opt.state = opt.state)

}
