proposePointsParEGO = function(opt.state) {
  weight.mat = attr(getOptStateTasks(opt.state), "weight.mat")
  models = getOptStateModels(opt.state)$models
  # copy control and propose 1 point each, per scalar task
  control2 = getOptProblemControl(getOptStateOptProblem(opt.state));
  control2$propose.points = 1L
  control2$n.objectives = 1L
  # scalar tasks are always constructed so they minimized
  control2$minimize = TRUE
  props = list()
  props = parallelMap(proposePointsByInfillOptimization, models = models, level = "mlrMBO.propose.points",
    more.args = list(opt.state = opt.state, control = control2))
  res = joinProposedPoints(props)
  res$crit.components = NULL
  res$weight.mat = weight.mat
  return(res)
}
