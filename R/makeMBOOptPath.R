# Create the opt.path with MBO-specific defaults -
# allways include error.message, exec.time and extra params, but never add transformed x.
#
# @param opt.problem [\code{OptProblem}]\cr
#   OptProblem Object
# @return [\code{\link[ParamHelpers]{optPath}}]
makeMBOOptPath = function(opt.problem) {
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  makeOptPathDF(
    par.set = par.set,
    y.names = control$y.name,
    minimize = control$minimize,
    add.transformed.x = FALSE,
    include.error.message = TRUE,
    include.exec.time = TRUE,
    include.extra = TRUE
  )
}
