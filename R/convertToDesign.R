# Takes x.vals and y.vals from the opt.path, possibly imputes missing x.vals and
# returns a data.frame.
#
# @param opt.state [OptState]
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
#   Might be altered for multipoint proposals etc.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
#   Might be altered for multipoint proposals etc.
# @return [\code{data.frame}]
#   With x and y columns

convertToDesign = function(opt.path, control) {
  df = as.data.frame(opt.path, include.rest = FALSE)
  convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
}
