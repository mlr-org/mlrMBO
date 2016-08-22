# Generates single objective task.
#
# - only uses x and y columns of optpath
# - converts data types for regr model
# - imputes features

# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param opt.problem [\code{\link{MBOControl}}]\cr
#   MBO opt.problem object.
# @return [\code{\link[mlr]{SupervisedTask}}]
makeTaskSingleObj = function(opt.path, opt.problem) {
  control = getOptProblemControl(opt.problem)
  data = convertOptPathToDf(opt.path, control)
  data$dob = data$eol = NULL
  y.name = control$y.name

  # user selected to (log)-transform the y-column
  trafo.y.fun = control$trafo.y.fun
  if (!is.null(trafo.y.fun)) {
    # We stop the process if negative values occur
    data[[y.name]] = trafo.y.fun(data[[y.name]])
  }

  makeRegrTask(target = control$y.name, data = data)
}
