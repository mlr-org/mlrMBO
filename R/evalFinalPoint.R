#@arg opt.state [OptState]
#@arg x.df [data.frame (1xp)]
#  The final point to be evaluated as a result from getOptPathX(op, best.index)
evalFinalPoint = function(opt.state, x.df) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  n = control$final.evals
  # do some final evaluations and compute mean of target fun values
  # FIXME: Do we really want the resampling of the last point be part of the opt.path and thus be part of a new model fit if we restart the problem?
  showInfo(getOptProblemShowInfo(opt.problem), "Performing %i final evals", n)
  x.df[seq_len(n), ] = x.df
  prop = makeProposal(
    control = control,
    prop.points = x.df,
    prop.type = rep("final_eval", n)
  )
  evalProposedPoints.OptState(opt.state, prop, train.time = NA_real_)
}
