# FIXME use other function instead of min for lie
multipointInfillOptCL = function(model, control, par.set, opt.path, design, ...) {
  learner = model$learner
  y.name = control$y.name
  # copy control
  control2 = control
  control2$propose.points = 1L
  newdes = data.frame()
  opt.path2 = opt.path
  #FIXME this is bad code
  opt.path2$env = new.env()
  opt.path2$env$path = opt.path$env$path
  opt.path2$env$dob = opt.path$env$dob
  opt.path2$env$eol = opt.path$env$eol
  lie = min(getOptPathY(opt.path, y.name))
  dob = max(getOptPathDOB(opt.path)) + 1
  crit.vals = data.frame()
  while (nrow(newdes) < control$propose.points) {
    newdes1 = proposePoints(model, par.set, control2, opt.path2)
    x = dfRowToList(newdes1$prop.points, par.set, 1)
    addOptPathEl(opt.path2, x = x, y = lie, dob = dob)
    newdes = rbind(newdes, newdes1$prop.points)
    crit.vals = c(crit.vals, newdes1$crit.vals)
    
    # update model  
    rt = makeMBOSingleObjTask(par.set, as.data.frame(opt.path2, discretes.as.factor = TRUE), control = control)
    model = train(learner, rt)
  }
  return(list(prop.points = newdes, crit.vals = crit.vals))
}
