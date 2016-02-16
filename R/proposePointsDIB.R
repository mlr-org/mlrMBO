# proposes 1 or multi-points via direct multiobjective indicator optimization
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
    # draw lambdas from exp dist + create 1 control for each for single crit with lambda-CB
    #FIXME: it is unclear whether we want also randomly sample lambdas here too
    # talked with TW and we agreed to not do this for now but only do the CL idea with adding points
    # if we only use the const lambda, we also dont need it in optpath
    z = createRandomCBControls(control, "dib", user.lambda = TRUE)
    props = list()
    # copy opt.path so we can store already proposed points in it
    opt.path2 = deepCopyOptPath(opt.path)
    dob = max(getOptPathDOB(opt.path)) + 1
    for (i in 1:control$propose.points) {
      control2 = z$controls[[i]]
      prop = proposePointsByInfillOptimization(opt.state, control = control2, opt.path = opt.path2)
      design = convertOptPathToDf(opt.path, control)
      cb = evalCritFunForMultiCritModels(infillCritCB, prop$prop.points, models, control2,
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
