multipointInfillOptCL = function(model, control, par.set, opt.path, design, ...) {
  learner = model$learner
  y.name = control$y.name
  k = control$propose.points
  # extract "liar", i. e., function used by cl method for lying
  liar = control$multipoint.cl.lie
  # copy control
  control2 = control
  control2$propose.points = 1L
  newdes = data.frame()
  opt.path2 = opt.path
  #FIXME: this is bad code
  opt.path2$env = new.env()
  opt.path2$env$path = opt.path$env$path
  opt.path2$env$dob = opt.path$env$dob
  opt.path2$env$eol = opt.path$env$eol
  lie = liar(getOptPathY(opt.path, y.name))
  dob = max(getOptPathDOB(opt.path)) + 1
  crit.vals = numeric(k)
  errors.model = character(k)
  for (i in 1:k) {
    prop = proposePoints(model, par.set, control2, opt.path2)
    x = dfRowToList(prop$prop.points, par.set, 1)
    addOptPathEl(opt.path2, x = x, y = lie, dob = dob)
    newdes = rbind(newdes, prop$prop.points)
    crit.vals[i] = prop$crit.vals
    errors.model[i] = prop$errors.model
    # update model
    rt = makeMBOSingleObjTask(par.set, as.data.frame(opt.path2, discretes.as.factor = TRUE), control = control)
    model = train(learner, rt)
  }
  return(list(prop.points = newdes, crit.vals = crit.vals, errors.model = errors.model))
}
