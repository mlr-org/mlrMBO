# Create the opt.path with MBO-specific defaults -
# allways include error.message, exec.time and extra params, but never add transformed x.
#
# @param opt.problem [\code{OptProblem}]\cr
#   OptProblem Object
# @return [\code{\link[ParamHelpers]{optPath}}]
makeMBOOptPath = function(opt.problem) {
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  if (!is.null(getOptProblemDriftParam(opt.problem))) {
    if (control$conceptdrift.learn.drift) {
      par.set = getOptProblemParSet(opt.problem, original.par.set = TRUE)
    }
    op = OptPathNgCd$new(
      drift.param = getOptProblemDriftParam(opt.problem),
      window.function = control$conceptdrift.window.function,
      par.set = par.set,
      y.names = control$y.name,
      minimize = control$minimize
    )
  } else {
    op = OptPathNg$new(par.set = par.set, y.names = control$y.name, minimize = control$minimize)
  }
  return(op)
}
