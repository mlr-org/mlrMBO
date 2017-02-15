# proposes 1 or multi-points via direct multi-objective indicator optimization
# the indicators we support are sms and epsilon
#
# 1-point proposal: we take the user-defined cb-lambda from the control, then simply optimize
#
# n-point proposal: we sample n random cb-lambdas, then propose a point normally,
#   then add it to the design (fake add) with the its cb-vector as fake output. then iterate.

proposePointsDIB = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  models = getOptStateModels(opt.state)$models
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  iter = getOptStateLoop(opt.state)

  if (control$propose.points == 1L) {
    res = proposePointsByInfillOptimization(opt.state)
  } else {
    # copy opt.path and set prop.points = 1
    control2 = control
    control2$propose.points == 1L
    # Create CB crit fun
    cbFun = makeMBOInfillCritCB(cb.lambda = control2$infill.crit$params$cb.lambda)$fun
    props = list()
    # copy opt.path so we can store already proposed points in it
    opt.path2 = deepCopyOptPath(opt.path)
    dob = max(getOptPathDOB(opt.path)) + 1
    for (i in 1:control$propose.points) {
      prop = proposePointsByInfillOptimization(opt.state, control = control2, opt.path = opt.path2)
      design = convertOptPathToDf(opt.path, control)
      cb = evalCritFunForMultiObjModels(cbFun, prop$prop.points, models, control2,
        par.set, design, iter)[1L, ]
      x = dfRowToList(prop$prop.points, par.set, 1)
      addOptPathEl(opt.path2, x = x, y = cb, dob = dob)
      props[[i]] = prop
    }
    res = joinProposedPoints(props)
    # store extra info
    # DH: don't to this for now
    # FIXME: do we want to sample or adapt lamda here? decide
    #res$multipoint.cb.lambdas = z$lambdas
  }
  return(res)
}
