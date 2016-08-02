# Takes x.vals and y.vals from the opt.path, possibly imputes missing x.vals and
# returns a data.frame.
#
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @param include.y [\code{logical(1)}]
#   Should the y col be included?
# @return [\code{data.frame}]
convertOptPathToDf = function(opt.path, control, include.y = TRUE) {
  df = as.data.frame(opt.path, include.rest = FALSE, include.y = include.y)
  df = convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
  return(df)
}
