getProposedPointsPriorities = function(prop, xs, opt.state) {
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  if (control$multipoint.method == "lcb" && control$schedule.priority == "explore") {
    #highest priority for those with highest lambda
    prop$multipoint.lcb.lambdas
  } else if (control$multipoint.method == "lcb" && control$schedule.priority == "exploit") {
    #highest priority for those with small lambda
    prop$multipoint.lcb.lambdas[order(prop$multipoint.lcb.lambdas)]
  } else if (control$multipoint.method == "lcb" && control$schedule.priority == "balanced") {
    #highest priority for those with lambda close to 1
    prop$multipoint.lcb.lambdas[order(abs(prop$multipoint.lcb.lambdas-1))]
  } else if (control$schedule.priority == "infill"){
    prop$crit.vals
  } else {
    stopf("Priority mehtod %s was not appliable!", control$schedule.priority)
  }
}