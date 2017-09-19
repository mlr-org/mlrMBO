# Resource-aware scheduling for parallel execution in an MBO-iteration via knapsack algorithm.
# Reduces CPU idle time and avoids model update delay.
#
# arguments should always be the same as in evalScheduleParallelMap.R
# see there for reference

evalScheduleKnapsack = function(wrapFun, xs, xs.trafo, xs.schedule.info = NULL, extras = NULL, opt.state) {
  if (!is.null(xs.schedule.info$times)) {
    control = getOptProblemControl(getOptStateOptProblem(opt.state))
    schedule.nodes = control$schedule.nodes
    
    # order everything according to priorities in xs.schedule.info
    order.idx = order(xs.schedule.info$priorities, decreasing = TRUE)
    xs = xs[order.idx]
    xs.trafo = xs.trafo[order.idx]
    xs.schedule.info = xs.schedule.info[order.idx,, drop = FALSE]
    extras = extras[order.idx]
    #delete Rows without full scheduling information
    del.na = complete.cases(xs.schedule.info)
    xs.schedule.info = xs.schedule.info[del.na,]
    xs = xs[del.na]
    xs.trafo = xs.trafo[del.na]
    extras = extras[del.na]
    
    # initialize variables
    occupied.time = double(length = schedule.nodes)
    scheduled = data.frame(
      job = integer(), #which job got scheduled
      on = integer(), #on which node is it scheduled
      at = double() #at what time is is scheduled
    )
    
    # the maximal runtime for each MBO iteration is defined by the runtime of the job with
    # the highest priority (t.max) based on the infill criterion.
    # remove jobs with runtime > t.max
    t.max = xs.schedule.info$times[1] * 1.05
    selection = xs.schedule.info$times <= t.max
    
    xs.schedule.info = xs.schedule.info[selection,]
    xs = xs[selection]
    xs.trafo = xs.trafo[selection]
    extras = extras[selection]
    
    x.times =  xs.schedule.info$times
    
    neg.time = which(x.times < 0)
    if (length(neg.time) > 0){
      x.times[neg.time] = max (x.times[neg.time] + xs.schedule.info$times.se[neg.time], min(x.times[x.times > 0]))
      t.max = max(t.max , x.times)
    }
    t.min = min(x.times)
    # no negative priorities
    min.prio = min(xs.schedule.info$priorities)
    if (min.prio < 0){
      x.prios = xs.schedule.info$priorities - min.prio * 1.01
    }else{
      x.prios =  xs.schedule.info$priorities
    }
    
    # if number of jobs is still > number of cores and the runtime difference 
    # between the jobs is big enough to map more than one job to a core
    # knapsack is used else schedule jobs directly
    ks.use = length(x.times) > schedule.nodes && t.max >= 2 * t.min
    if (ks.use){
      no.jobs = length(x.times)						
      no.nodes = schedule.nodes
      
      # convert variables for Knapsack(integer)
      max.time = rep(t.max, times = no.nodes) 
      if(typeof(x.prios) != "integer"){
        tmp = round(.Machine$integer.max / (1000 * (sum(x.prios))))    
        ks.prio = as.integer(x.prios * tmp)
      }else{
        ks.prio = x.prios
      }
      ks.time = as.integer(x.times * 1000)
      ks.cap = as.integer(max.time * 1000)
      if (requireNamespace("adagio")){
        ks.vector = adagio::mknapsack(ks.prio, ks.time, ks.cap, bck = 50)
      }else{
        stop("Package adagio is needed for Knapsack scheduling. Please install it", call. = FALSE)
      }
      
      # schedule where(on) which job(job) will be executed at which time(at)	according to priorities
      for (i in 1:no.jobs){
        if(ks.vector$ksack[i] != 0){
          j =  ks.vector$ksack[i] 
          scheduled = rbind(scheduled,list(job = i,on = j, at = occupied.time[j]))
          occupied.time[j] = occupied.time[j] + xs.schedule.info$times[i]
        }
      }
      }else{
      # if number of jobs =< number of cpus, schedule directly by priorities.
        no.jobs = length(x.times)
        job.sel = numeric(no.jobs)
        job.sel[1:schedule.nodes] = 1:schedule.nodes
        for (i in 1:no.jobs){
          if(job.sel[i] != 0){
            j =  job.sel[i] 
            scheduled = rbind(scheduled,list(job = i,on = j, at = occupied.time[j]))
            occupied.time[j] = occupied.time[j] + xs.schedule.info$times[i]
          }
        }
      }	
    
    # reorder jobs to suit the load balancer
    scheduled = scheduled[order(scheduled$on),]
    load.balance.order = order(scheduled$at, decreasing = FALSE)
    scheduled = scheduled[load.balance.order,]
    xs = xs[scheduled$job]
    xs.trafo = xs.trafo[scheduled$job]
    xs.schedule.info = xs.schedule.info[scheduled$job,]
    
    extras = extras[scheduled$job]
    
    # Fill empty Nodes with Random Jobs# Fill empty Nodes with Random Jobs
    if (control$schedule.fill.random && (empty.slots = schedule.nodes - nrow(scheduled))>0) {
      stuff = fillRandom(t.max = t.max, empty.slots = empty.slots, opt.state = opt.state)
      used.slots = length(stuff[[1]])
      if (used.slots>0){  
        scheduled = rbind(scheduled, list(job = length(xs) + seq_along(1:used.slots), on = max(scheduled$on) + seq_along(1:used.slots), at = occupied.time[max(scheduled$on) + seq_along(1:used.slots)]))
        xs = c(xs, stuff[[1]])
        xs.trafo = c(xs.trafo, stuff[[2]])
        extras = c(extras, stuff[[3]])
      }
    }
    #put scheduling information into extras
    for (i in seq_row(scheduled)) {
      extras[[i]]$scheduled.job = scheduled$job[i]
      extras[[i]]$scheduled.on = scheduled$on[i]
      extras[[i]]$scheduled.at = scheduled$at[i]  
      extras[[i]]$scheduled.priority = xs.schedule.info$priorities[i]
      extras[[i]]$ks.used = ks.use
    }
  }
  evalScheduleParallelMap(wrapFun = wrapFun, xs = xs, xs.trafo = xs.trafo, xs.schedule.info = xs.schedule.info, extras = extras, opt.state = opt.state)
}
