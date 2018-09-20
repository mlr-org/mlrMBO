evalAdvancedScheduling = function(wrapFun, xs, xs.trafo, xs.schedule.info = NULL, extras = NULL, opt.state) {

  if (!is.null(xs.schedule.info$times)) {
    control = getOptProblemControl(getOptStateOptProblem(opt.state))
    schedule.nodes = control$schedule.nodes
    
    t.max = xs.schedule.info$t.max[1]
    
    # schedule where(on) which job(job) will be executed at which time(at)
    occupied.time = double(length = schedule.nodes)
    scheduled = data.frame(
      job = integer(), #which job got scheduled
      on = integer(), #on which node is it scheduled
      at = double(), #at what time is is scheduled
      wait.at = double()
    )
    ## Scheduling
    i = 1 # job
    njobs = length(xs)
    scheduled.on = vector("list", njobs)
    for (j in seq_len(schedule.nodes)){
      while(i <= njobs){
        if (t.max - occupied.time[j] >= xs.schedule.info$times[i]) {
          scheduled = rbind(scheduled, list(job = i, on = j, at = occupied.time[j], wait.at = Inf))
          scheduled.on[[i]] = j
          occupied.time[j] = occupied.time[j] + xs.schedule.info$times[i]
          i = i + 1
        }else{
          time.on.j = t.max - occupied.time[j]
          wait.at = xs.schedule.info$times[i] - time.on.j
          scheduled = rbind(scheduled, list(job = i, on = j+1, at = occupied.time[j+1], wait.at = wait.at))
          scheduled.on[[i]] = c(j+1, j)
          occupied.time[j] = occupied.time[j] + time.on.j
          occupied.time[j+1] = occupied.time[j+1] + wait.at
          i = i + 1
          break()
        }
      }
    }
    ##
    # reorder jobs for better load balancing
    load.balance.order = order(scheduled$at, decreasing = FALSE)
    scheduled.on = scheduled.on[load.balance.order]
    scheduled = scheduled[load.balance.order,]
    xs = xs[scheduled$job]
    xs.trafo = xs.trafo[scheduled$job]
    xs.schedule.info = xs.schedule.info[scheduled$job,]
    extras = extras[scheduled$job]
    
    # put scheduling information into extras
    for (i in seq_row(scheduled)) {
      extras[[i]]$scheduled.job = scheduled$job[i]
      extras[[i]]$scheduled.on = scheduled$on[i]
      extras[[i]]$scheduled.at = scheduled$at[i]  
    }
    

    funRes = alapply(X = xs.trafo, scheduled.on = scheduled.on, wait.at = scheduled$wait.at, FUN = wrapFun)
    
    # fix times for paused tasks
    for (i in seq_along(funRes)) {
      if (!is.null(funRes[[i]]$stime) && funRes[[i]]$stime > 0){
        extras[[i]]$stop.time = funRes[[i]]$stime
        extras[[i]]$cont.time = funRes[[i]]$ctime
      }
    }
  } else{

    imputeY = getOptProblemControl(
      getOptStateOptProblem(opt.state))$impute.y.fun
    
    funRes = parallelMap(wrapFun, xs.trafo, level = "mlrMBO.feval",
                         impute.error = if (is.null(imputeY)) NULL else identity)
    
  }
  list(funRes = funRes, xs = xs, xs.trafo = xs.trafo, extras = extras, dob = getOptStateLoop(opt.state))
}