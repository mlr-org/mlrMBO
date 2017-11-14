# Takes x.vals and y.vals from the opt.path, possibly imputes missing x.vals and
# returns a data.frame.
#
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{data.frame}]
convertOptPathToDf = function(x, ...) {
  UseMethod("convertOptPathToDf")
}

convertOptPathToDf.OptState = function(opt.state, opt.path, control) {
  if (missing(opt.state) || is.null(opt.state)) {
    df = convertOptPathToDf(opt.path, control)
    return(df)
  }
  opt.path = getOptStateOptPath(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  # FIXME Fix for Issue https://github.com/mlr-org/mlrMBO/issues/407
  # Maybe there is a better way?
  if (control$multiobj.method == "parego" && isTRUE(control$multiobj.use.scalarized.y)) {
    res = generateParEgoDfData(opt.state, opt.path, control)
    df = generateParEgoDf(res, lambda = res$lambdas[control$multiobj.parego.lambda.ind])
  } else {
    df = convertOptPathToDf(opt.path, control)
  }
  return(df)
}

convertOptPathToDf.OptPath = function(opt.path, control) {
  df = as.data.frame(opt.path, include.rest = FALSE)
  convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
}
