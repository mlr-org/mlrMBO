proposePointsParallelCB = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  # draw lambdas from exp dist + create 1 control for each for single crit with lambda-CB
  z = createRandomCBControls(control, "cb")

  props = parallelMap(proposePointsByInfillOptimization, control = z$controls, level = "mlrMBO.propose.points",
    more.args = list(opt.state = opt.state))

  res = joinProposedPoints(props)
  res$multipoint.cb.lambdas = z$lambdas
  return(res)
}
