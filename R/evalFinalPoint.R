evalFinalPoint = function(opt.state, final.points) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  final.evals = control$final.evals
  # do some final evaluations and compute mean of target fun values
  # FIXME: Do we really want the resampling of the last point be part of the opt.path and thus be part of a new model fit if we restart the problem?
  showInfo(getOptProblemShowInfo(opt.problem), "Performing %i final evals", final.evals)
  xs = replicate(final.evals, final.points$x, simplify = FALSE)
  extras = getExtras(final.evals, NULL, NA_real_, control)
  evalTargetFun.OptState(opt.state, xs, extras)
}
