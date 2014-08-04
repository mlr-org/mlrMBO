# Create the opt.path with MBO-specific defaults - 
# allways include error.message, exec.time and extra params, but never add transformed x.
#
# @param par.set [\code{param.set}]\cr
#   Parameter set.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{\link[ParamHelpers]{optPath}}]
makeMBOOptPath = function(par.set, control) {
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

