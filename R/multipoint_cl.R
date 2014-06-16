# FIXME use other function instead of min for lie
# FIXME do we always want ei?
# FIXME add unit test
multipointInfillOptCL = function(model, control, par.set, opt.path, design, ...) {
  learner = model$learner
  y.name = control$y.name
  # copy control
  control2 = control
  control2$propose.points = 1L
  control2$infill.crit = "ei"
  newdes = data.frame()
  op2 = opt.path
  #FIXME this is bad code
  op2$env = new.env()
  op2$env$path = opt.path$env$path
  op2$env$dob = opt.path$env$dob
  op2$env$eol = opt.path$env$eol
  lie = min(getOptPathY(opt.path, y.name))
  dob = max(getOptPathDOB(opt.path)) + 1
  prop.points.crit.values = data.frame()
  while (nrow(newdes) < control$propose.points) {
    newdes1 = proposePoints(model, par.set, control2, op2)
    x = dfRowToList(newdes1$prop.points, par.set, 1)
    addOptPathEl(op2, x = x, y = lie, dob = dob)
    newdes = rbind(newdes, newdes1$prop.points)
    prop.points.crit.values=c(prop.points.crit.values, newdes1$prop.points.crit.values)
    
    # update model
    rt = makeMBOSingleObjTask(as.data.frame(op2, discretes.as.factor = TRUE),
      par.set, control = control)
    model = train(learner, rt)
  }
  return(list(prop.points = newdes, prop.points.crit.values = prop.points.crit.values))
}
