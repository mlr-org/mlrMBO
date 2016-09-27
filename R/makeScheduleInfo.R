makeScheduleInfo = function(prop, opt.state) {
  #calculate priorities
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  if (control$multipoint.method == "cb" && control$schedule.priority == "explore") {
    #highest priority for those with highest lambda
    priorities = prop$multipoint.cb.lambdas
  } else if (control$multipoint.method == "cb" && control$schedule.priority == "exploit") {
    #highest priority for those with small lambda
    priorities = -prop$multipoint.cb.lambdas
  } else if (control$multipoint.method == "cb" && control$schedule.priority == "balanced") {
    #highest priority for those with lambda close to desired lambda
    priorities =  -abs(log(prop$multipoint.cb.lambdas) - log(control$infill.crit.cb.lambda))
  } else if (control$schedule.priority == "infill"){
    #highest priority for those with lowest crit.val
    priorities = -prop$crit.vals
  } else {
    stopf("Schedule Priority mehtod %s was not appliable!", control$schedule.priority)
  }
  if (control$schedule.cluster && !control$schedule.priority.time){
    #highest priority for those with highest priority within each cluster
    priorities = priorityCluster(priorities, prop, opt.state)
  }
  if (control$schedule.cluster && control$schedule.priority.time){
    #highest priority for those with highest priority within each cluster
    priorities = timeCluster(priorities, prop, opt.state)
  }
  
  if (control$schedule.priority.time && !control$schedule.cluster) {
    first.id = which.max(priorities)
    priorities = -prop$predicted.time
    priorities[first.id] = max(priorities) + 1
  }
  data.frame(
    times = prop$predicted.time,
    priorities = priorities,
    times.se = prop$predicted.time.se)
}
