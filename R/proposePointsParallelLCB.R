proposePointsParallelLCB = function(models, par.set, control, opt.path, iter) {
  # draw lambdas from exp dist + create 1 control for each for single crit with lambda-LCB
  z = createRandomLCBControls(control, "lcb")

  props = parallelMap(proposePointsByInfillOptimization, control = z$controls, level = "propose.points",
    more.args = list(models = models[[1L]], par.set = par.set, opt.path = opt.path, iter = iter))

  res = joinProposedPoints(props)
  res$multipoint.lcb.lambdas = z$lambdas
  return(res)
}

