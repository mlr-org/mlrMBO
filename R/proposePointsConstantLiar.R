proposePointsConstantLiar = function(models, par.set, control, opt.path, iter, ...) {
  model = models[[1L]]
  npoints = control$propose.points
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
  lie = liar(getOptPathY(opt.path, control$y.name))
  dob = max(getOptPathDOB(opt.path)) + 1
  props = list()
  for (i in 1:npoints) {
    props[[i]] = proposePointsByInfillOptimization(list(model), par.set, control2, opt.path2, models.unlist = TRUE)
    x = dfRowToList(props[[i]]$prop.points, par.set, 1)
    addOptPathEl(opt.path2, x = x, y = lie, dob = dob)
    # update model
    rt = makeTaskSingleObj(par.set, opt.path2, control)
    model = train(model$learner, rt)
  }
  list(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE))
  )
}

