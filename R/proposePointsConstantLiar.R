proposePointsConstantLiar = function(models, par.set, control, opt.path, iter) {
  model = models[[1L]]
  npoints = control$propose.points
  liar = control$multipoint.cl.lie
  # copy control, and propose 1 point each
  control2 = control
  control2$propose.points = 1L
  # copy opt.path to store lies
  opt.path2 = deepCopyOptPath(opt.path)
  lie = liar(getOptPathY(opt.path, control$y.name))
  dob = max(getOptPathDOB(opt.path)) + 1
  props = list()
  for (i in 1:npoints) {
    # propose point, add to opt.path2 with y = lie, then update model
    props[[i]] = proposePointsByInfillOptimization(model, par.set, control2, opt.path2)
    x = dfRowToList(props[[i]]$prop.points, par.set, 1)
    addOptPathEl(opt.path2, x = x, y = lie, dob = dob)
    rt = makeTaskSingleObj(par.set, opt.path2, control)
    model = train(model$learner, rt)
  }
  joinProposedPoints(props)
}

