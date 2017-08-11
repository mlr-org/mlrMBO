evalFinalPoint = function(opt.state, final.points) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  n = control$final.evals
  # do some final evaluations and compute mean of target fun values
  # FIXME: Do we really want the resampling of the last point be part of the opt.path and thus be part of a new model fit if we restart the problem?
  showInfo(getOptProblemShowInfo(opt.problem), "Performing %i final evals", n)
  xs = replicate(n, final.points$x, simplify = FALSE)
  prop = makeProposal(
    control = control,
    prop.points = xs,
    prop.type = rep("final_eval", n)
  )
  extras = getExtras(n = n, prop = prop, train.time = NA_real_, control = control)
  evalTargetFun.OptState(opt.state, xs = xs, extras = extras)
}
