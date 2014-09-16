proposePointsParallelLCB = function(models, par.set, control, opt.path, iter) {
  # draw lambdas from exp dist + create 1 control for each for single crit with lambda-LCB
  lambdas = rexp(control$propose.points)
  controls = lapply(lambdas, function(lambda) {
    ctrl = control;
    ctrl$propose.points = 1L;
    ctrl$infill.crit = "lcb"
    ctrl$infill.crit.lcb.lambda = lambda
    return(ctrl)
  })

  props = parallelMap(proposePointsByInfillOptimization, controls,
    more.args = list(models = models[[1L]], par.set = par.set, opt.path = opt.path, iter = iter))

  joinProposedPoints(props)
}

