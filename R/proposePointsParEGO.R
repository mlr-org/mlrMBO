proposePointsParEGO = function(models, par.set, control, opt.path, iter, weight.mat) {
  # copy control and propose 1 point each, per scalar task
  control2 = control;
  control2$propose.points = 1L
  control2$number.of.targets = 1L
  # scalar tasks are always constructed so they minimized
  control2$minimize = TRUE
  props = list()
  props = parallelMap(proposePointsByInfillOptimization, models, level = "propose.points",
    more.args = list(par.set = par.set, control = control2, opt.path = opt.path, iter = iter))

  res = joinProposedPoints(props)
  res$weight.mat = weight.mat
  return(res)
}
