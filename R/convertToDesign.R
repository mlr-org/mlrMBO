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

convertToDesign = function(x, ...) {
  UseMethod("convertToDesign")
}


convertToDesign.OptState = function(opt.state, opt.path, control) {
  if (missing(opt.state) || is.null(opt.state)) {
    df = convertToDesign(opt.path, control)
    return(df)
  }

  opt.path = opt.path %??% getOptStateOptPath(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  control = control %??% getOptProblemControl(opt.problem)

  # FIXME Fix for Issue https://github.com/mlr-org/mlrMBO/issues/407
  # Maybe there is a better way?
  if (control$multiobj.method == "parego" && isTRUE(control$multiobj.use.scalarized.y)) {
    res = generateParEgoDfData(opt.state, opt.path, control)
    df = generateParEgoDf(res, lambda = res$lambdas[control$multiobj.parego.lambda.ind])
  } else {
    df = convertToDesign(opt.path, control)
  }
  return(df)
}

convertToDesign.OptPath = function(opt.path, control) {
  df = as.data.frame(opt.path, include.rest = FALSE)
  convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
}

convertToDesign.Task = function(task) {
  getTaskData(task)
}