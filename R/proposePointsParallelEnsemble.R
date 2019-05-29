proposePointsParallelEnsemble = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  controls = createSinglePointControls(opt.problem, control$multipoint.ensemble.crits)

  props = parallelMap(proposePointsByInfillOptimization, control = controls, level = "mlrMBO.propose.points",
    more.args = list(opt.state = opt.state))

  for (i in seq_along(props)) {
    props[[i]]$crit.components$ensemble = i #prevents empty df (problematic with rbindlist) and adds some info
  }

  res = joinProposedPoints(props)
  res$multipoint.crit.id = vapply(control$multipoint.ensemble.crits, function(x) x$id, character(1L))
  return(res)
}
