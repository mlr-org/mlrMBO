proposePointsConstantLiar = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  model = getOptStateModels(opt.state)$models[[1L]]
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)

  npoints = control$propose.points
  # copy control, and propose 1 point each
  control2 = control
  control2$propose.points = 1L
  # copy opt.path to store lies
  opt.path2 = deepCopyOptPath(opt.path)
  dob = max(getOptPathDOB(opt.path)) + 1
  props = list()
  for (i in 1:npoints) {
    # propose point, add to opt.path2 with y = lie, then update model
    props[[i]] = proposePointsByInfillOptimization(opt.state, control = control2, opt.path = opt.path2, models = list(model))
    x = dfRowToList(props[[i]]$prop.points, par.set, 1)
    addOptPathEl(opt.path2, x = x, y = liar(opt.problem, opt.path2, props[[i]]$prop.points, model), dob = dob)
    rt = makeTaskSingleObj(opt.path2, control)
    model = train(model$learner, rt)
  }
  joinProposedPoints(props)
}

liar = function(opt.problem, opt.path, x, model = NULL) {
  control = getOptProblemControl(opt.problem)
  if (control$multipoint.mean.liar) {
    if (is.null(model)) {
      model = train(getOptProblemLearner(opt.problem), makeTaskSingleObj(opt.path, control))
    }
    res = getPredictionResponse(predict(model, newdata = x))
    stopf("I lied this value %.3f", res)
  } else {
    res = control$multipoint.cl.lie(getOptPathY(opt.path, control$y.name))
  }
  return(res)
}
