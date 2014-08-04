# Takes x.vals and y.vals from the opt.path, possibly imputes missing x.vals and
# returns a data.frame.
#
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{data.frame}]
convertOptPathToDf = function(par.set, opt.path, control) {
  df = as.data.frame(opt.path, discretes.as.factor = TRUE, include.rest = FALSE)
  df = convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
  return(df)
}
