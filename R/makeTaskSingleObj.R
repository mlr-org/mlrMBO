# Generates single-objective task.
#
# - only uses x and y columns of optpath
# - converts data types for regr model
# - imputes features

# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{\link[mlr]{SupervisedTask}}]
makeTaskSingleObj = function(opt.path, control) {
  data = convertToDesign(opt.path, control)
  data$dob = data$eol = NULL

  # user selected to (log)-transform the y-column
  trafo.y.fun = control$trafo.y.fun
  if (!is.null(trafo.y.fun)) {
    y.name = control$y.name
    # We stop the process if negative values occur
    data[[y.name]] = trafo.y.fun(data[[y.name]])
  }

  makeRegrTask(target = control$y.name, data = data)
}
