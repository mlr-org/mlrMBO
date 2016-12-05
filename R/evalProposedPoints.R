# evals the points proposed by proposePoints
# (called by mboTemplate)
#
# @param opt.state
# @param prop: result of proposePoints
# @return [\code{numeric} | \code{matrix}] Numeric vector of y-vals or matrix
#   (for multi-objective problems).
#
# gets the getExtras, converts point data.frame to a list, repairs points out-of-bounds
# then call evalTargetFun

evalProposedPoints.OptState = function(opt.state, prop) {
  opt.problem = getOptStateOptProblem(opt.state)
  par.set = getOptProblemParSet(opt.problem)
  extras = getExtras(
    n = nrow(prop$prop.points),
    prop = prop,
    train.time = getOptStateModels(opt.state)$train.time,
    control = getOptProblemControl(opt.problem)
  )
  xs = dfRowsToList(prop$prop.points, par.set)
  xs = lapply(xs, repairPoint, par.set = par.set)
  evalTargetFun.OptState(opt.state, xs = xs, extras = extras)
}
