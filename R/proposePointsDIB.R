# proposes 1 or multi-points via direct multiobjective indicator optimization
# the indicators we support are sms and epsilon
#
# 1-point proposal: we take the user-defined lcb-lambda from the control, then simply optimize
#
# n-point proposal: we sample n random lcb-lambdas, then propose a point normally,
#   then add it to the design (fake add) with the its lcb-vector as fake output. then iterate.

proposePointsDIB = function(models, par.set, control, opt.path, iter) {
  if (control$propose.points == 1L) {
    res = proposePointsByInfillOptimization(models = models, control = control,
        par.set = par.set, opt.path = opt.path, iter = iter)
  } else {
    # draw lambdas from exp dist + create 1 control for each for single crit with lambda-LCB
    z = createRandomLCBControls(control, "dib")
    props = list()
    # copy opt.path so we can store already proposed points in it
    opt.path2 = deepCopyOptPath(opt.path)
    dob = max(getOptPathDOB(opt.path)) + 1
    for (i in 1:control$propose.points) {
      control2 = z$controls[[i]]
      prop = proposePointsByInfillOptimization(models = models, control = control2,
        par.set = par.set, opt.path = opt.path2, iter = iter)
      lcb = evalCritFunForMultiCritModels(infillCritLCB, prop$prop.points, models, control2,
        par.set, design, iter)[1L, ]
      x = dfRowToList(prop$prop.points, par.set, 1)
      addOptPathEl(opt.path2, x = x, y = lcb, dob = dob)
      props[[i]] = prop
    }
    res = joinProposedPoints(props)
    # store extra info
    res$multipoint.lcb.lambdas = z$lambdas
  }
  return(res)
}



