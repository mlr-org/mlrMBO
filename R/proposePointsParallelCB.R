proposePointsParallelCB = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  # draw lambdas from exp dist + create 1 control for each for single-objective with lambda-CB
  lambdas = rexp(control$propose.points)
  controls = createSinglePointControls(control, makeMBOInfillCritCB, crit.pars = .mapply(list, list(cb.lambda = lambdas), list()))

  props = parallelMap(proposePointsByInfillOptimization, control = controls, level = "mlrMBO.propose.points",
    more.args = list(opt.state = opt.state))

  res = joinProposedPoints(props)
  res$multipoint.cb.lambdas = lambdas
  return(res)
}
