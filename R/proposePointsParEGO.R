proposePointsParEGO = function(opt.state) {
  weight.mat = attr(getOptStateTasks(opt.state), "weight.mat")
  models = getOptStateModels(opt.state)$models
  # copy control and propose 1 point each, per scalar task
  control2 = getOptProblemControl(getOptStateOptProblem(opt.state));
  control2$propose.points = 1L
  control2$n.objectives = 1L
  # scalar tasks are always constructed so they minimized
  control2$minimize = TRUE
  control2$multiobj.use.scalarized.y = TRUE

  controls = lapply(seq_len(control2$propose.points), function(z) {
    control2$multiobj.parego.lambda.ind = z
    return(control2)
  })
  
  props = list()
  props = parallelMap(proposePointsByInfillOptimization, models = models, control = controls, level = "mlrMBO.propose.points",
    more.args = list(opt.state = opt.state))
  res = joinProposedPoints(props)
  res$crit.components = NULL
  res$weight.mat = weight.mat
  return(res)
}
