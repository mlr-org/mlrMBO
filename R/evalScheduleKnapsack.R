# arguments should always be the same as in evalScheduleParallelMap.R
# see there for reference

evalScheduleKnapsack = function(wrapFun, xs, xs.trafo, xs.schedule.info = NULL, extras = NULL, opt.state) {
	if (!is.null(xs.schedule.info$times)) {
		control = getOptProblemControl(getOptStateOptProblem(opt.state))
		schedule.nodes = control$schedule.nodes
		
		# order everything according to times in xs.schedule.info
		if (!is.null(xs.schedule.info$priorities)) {
		  order.idx = order(xs.schedule.info$times, decreasing = FALSE)
		  xs = xs[order.idx]
		  xs.trafo = xs.trafo[order.idx]
		  xs.schedule.info = xs.schedule.info[order.idx,, drop = FALSE]
		  extras = extras[order.idx]
		}
		
		# initialize Variables
		occupied.time = double(length = schedule.nodes)
		scheduled = data.frame(
		  job = integer(), #which job got scheduled
		  on = integer(), #on which node is it scheduled
		  at = double() #at what time is is scheduled
		)
		pos.max.prio = which.max(xs.schedule.info$priorities)
		t.max = xs.schedule.info$times[pos.max.prio] * 1.05
		x.times =  xs.schedule.info$times[xs.schedule.info$times <= t.max]

		# if more than one job is available for scheduling use knapsack
		# else schedule it direct
		if (length(x.times)>=2){
		  #t.max = xs.schedule.info$times[pos.max.prio+1] + 0.05 * xs.schedule.info$times[pos.max.prio]
		  #x.times =  xs.schedule.info$times[xs.schedule.info$times <= t.max]
		
		  no.jobs = length(x.times)						
		  no.nodes = schedule.nodes
		  # no negative priorities
		  min.prio = min(xs.schedule.info$priorities[1:no.jobs])
		  if (min.prio < 0){
		    x.prios = xs.schedule.info$priorities[1:no.jobs] - min.prio * 1.01  #FIXME: nicer?  
		  }else{
		    x.prios =  xs.schedule.info$priorities[1:no.jobs] 
		  }	
		  max.time = rep(t.max,times=no.nodes) 
		
		  # convert variables for Knapsack(integer)
		  tmp = round(.Machine$integer.max / (100*sum(x.prios)))
		  ks.prio = as.integer(x.prios*tmp)
		  ks.time = as.integer(x.times*1000)
		  ks.cap = as.integer(max.time*1000)
		  if (requireNamespace("adagio")){
		    ks.vector = adagio::mknapsack(ks.prio, ks.time, ks.cap, bck = 10000)
		  }else{
		    stop("Package adagio is needed for Knapsack scheduling. Please install it", call. = FALSE)
		  }
		  # schedule where(on) which job(job) will be executed at which time(at)	

		  for (i in 1:no.jobs){
		    if(ks.vector$ksack[i] != 0){
		      j =  ks.vector$ksack[i] 
		      scheduled = rbind(scheduled,list(job = i,on = j, at = occupied.time[j]))
		      occupied.time[j] = occupied.time[j] + xs.schedule.info$times[i]
		    }
		  }
		}else{
		  # schedule where(on) which job(job) will be executed at which time(at)
		  scheduled = rbind(scheduled,list(job = 1, on = 1, at = 0))
		}	
		# reorder jobs to suit the load balancer
		load.balance.order = order(scheduled$at, decreasing = FALSE)
		scheduled = scheduled[load.balance.order,]
		xs = xs[scheduled$job]
		xs.trafo = xs.trafo[scheduled$job]
		xs.schedule.info[scheduled$job,]
		extras = extras[scheduled$job]
		
		#put scheduling information into extras
		for (i in seq_row(scheduled)) {
			extras[[i]]$scheduled.job = scheduled$job[i]
			extras[[i]]$scheduled.on = scheduled$on[i]
			extras[[i]]$scheduled.at = scheduled$at[i]  
		  extras[[i]]$scheduled.priority = xs.schedule.info$priorities[i]
		}
	}
	evalScheduleParallelMap(wrapFun = wrapFun, xs = xs, xs.trafo = xs.trafo, xs.schedule.info = xs.schedule.info, extras = extras, opt.state = opt.state)
}
