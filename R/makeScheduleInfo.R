makeScheduleInfo = function(prop, opt.state) {
  # calculate priorities
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  
  if (control$multipoint.method == "groupQKP"){
    res = data.frame(
      times = prop$predicted.time,
      times.se = prop$predicted.time.se, 
      t.max = prop$t.max)
    return(res)
  }
  
    if (control$multipoint.method == "cb" && control$schedule.priority == "explore") {
    # highest priority for those with highest lambda
    priorities = prop$multipoint.cb.lambdas
  } else if (control$multipoint.method == "cb" && control$schedule.priority == "exploit") {
    # highest priority for those with small lambda
    priorities = -prop$multipoint.cb.lambdas
  } else if (control$multipoint.method == "cb" && control$schedule.priority == "balanced") {
    # highest priority for those with lambda close to desired lambda
    priorities =  -abs(log(prop$multipoint.cb.lambdas) - log(control$infill.crit.cb.lambda))
  } else if(control$multipoint.method == "cb" && control$schedule.priority == "raw"){
    # highest priority for those with low yhat
    priorities = -prop$crit.components$mean
  } else if (control$schedule.priority == "infill"){
    # highest priority for those with lowest crit.val
    priorities = -prop$crit.vals
  }else {
    stopf("Schedule Priority mehtod %s was not appliable!", control$schedule.priority)
  }

  # adjust priority via clustering if desired
  priorities = switch(control$schedule.cluster,
         priority = priorityCluster(priorities, prop, opt.state),
         time = timeCluster(priorities, prop, opt.state),
         distance = distanceCluster(priorities, prop, opt.state),
         none = priorities
         )
  
  # repair negative times
  # replace negative times with smallest observed time or smallest postive predicted time
  predicted.times = prop$predicted.time
  min.time = min(predicted.times[predicted.times > 0], getOptPathExecTimes(getOptStateOptPath(opt.state)), na.rm = TRUE)
  predicted.times[predicted.times <= 0] = min.time
  
  if (control$schedule.priority.time) {
    first.id = which.max(priorities)
    priorities = -predicted.times
    priorities[first.id] = max(priorities) + 1
  }
  data.frame(
    times = predicted.times,
    priorities = priorities,
    times.se = prop$predicted.time.se)
}
