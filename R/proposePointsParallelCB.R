proposePointsParallelCB = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  # draw lambdas from exp dist + create 1 control for each for single-objective with lambda-CB
  lambdas = rexp(control$propose.points)
  crits = lapply(lambdas, function(lambda) {
    makeMBOInfillCritCB(cb.lambda = lambda)
  })
  controls = createSinglePointControls(opt.problem, crits)

  props = parallelMap(proposePointsByInfillOptimization, control = controls, level = "mlrMBO.propose.points",
    more.args = list(opt.state = opt.state))

  res = joinProposedPoints(props)
  res$multipoint.cb.lambdas = lambdas
  return(res)
}
