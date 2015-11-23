makeScheduleInfo = function(prop, opt.state) {
  #calculate priorities
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  if (control$multipoint.method == "lcb" && control$schedule.priority == "explore") {
    #highest priority for those with highest lambda
    priorities = prop$multipoint.lcb.lambdas
  } else if (control$multipoint.method == "lcb" && control$schedule.priority == "exploit") {
    #highest priority for those with small lambda
    priorities = prop$multipoint.lcb.lambdas[order(prop$multipoint.lcb.lambdas)]
  } else if (control$multipoint.method == "lcb" && control$schedule.priority == "balanced") {
    #highest priority for those with lambda close to 1
    priorities =  prop$multipoint.lcb.lambdas[order(abs(log(prop$multipoint.lcb.lambdas)-log(1)))]
  } else if (control$schedule.priority == "infill"){
    priorities = prop$crit.vals
  } else {
    stopf("Priority mehtod %s was not appliable!", control$schedule.priority)
  }
  data.frame(
    times = prop$predicted.time,
    priorities = priorities,
    times.se = prop$predicted.time.se)
}
