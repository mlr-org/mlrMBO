proposePointsConstantLiar = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  model = getOptStateModels(opt.state)$models[[1L]]
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)

  npoints = control$propose.points
  liar = control$multipoint.cl.lie
  # copy control, and propose 1 point each
  control2 = control
  control2$propose.points = 1L
  # copy opt.path to store lies
  opt.path2 = deepCopyOptPath(opt.path)
  lie = liar(getOptPathY(opt.path, control$y.name))
  dob = max(getOptPathDOB(opt.path)) + 1L
  props = list()
  for (i in seq_len(npoints)) {
    # propose point, add to opt.path2 with y = lie, then update model
    props[[i]] = proposePointsByInfillOptimization(opt.state, control = control2, opt.path = opt.path2, models = list(model))
    if (i==npoints) break # we don't need to update the model when we aleady have the n-th proposal
    x = dfRowToList(props[[i]]$prop.points, par.set, 1)
    addOptPathEl(opt.path2, x = x, y = lie, dob = dob)
    rt = makeTaskSingleObj(opt.path2, control)
    model = train(model$learner, rt)
  }
  joinProposedPoints(props)
}
