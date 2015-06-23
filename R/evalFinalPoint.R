evalFinalPoint = function(tuningState, final.points) {
  tuningProblem = getTuningStateTuningProblem(tuningState)
  control = getTuningProblemControl(tuningProblem)
  final.evals = control$final.evals 
  # do some final evaluations and compute mean of target fun values
  # FIXME: Do we really want the resampling of the last point be part of the opt.path and thus be part of a new model fit if we restart the problem?
  showInfo(getTuningProblemShowInfo(tuningProblem), "Performing %i final evals", final.evals)
  xs = replicate(final.evals, final.points$x, simplify = FALSE)
  extras = getExtras(final.evals, NULL, NA_real_, control)
  evalTargetFun.TuningState(tuningState, xs, extras)
}