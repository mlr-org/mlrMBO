# Generates multi objective tasks - one for each target variable.
#
# - only uses x and y columns of optpath
# - converts data types for regr model
# - imputes features
#
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{list(\link[mlr]{SupervisedTask}})]
makeTasksMultiObj = function(opt.path, control) {
  data = convertToDesign(opt.path, control)

  # FIXME: trafo.y.fun should be a list of length y.name
  # user selected to (log)-transform the y-column
  trafo.y.fun = control$trafo.y.fun
  if (!is.null(trafo.y.fun)) {
    y.name = control$y.name
    # We stop the process if negative values occur
    data[, y.name] = trafo.y.fun(data[, y.name])
  }

  tasks = vector(mode = "list", length = control$n.objectives)
  for (ind in seq_along(control$y.name)) {
    data.tmp = dropNamed(data, control$y.name[-ind])
    tasks[[ind]] = makeRegrTask(target = control$y.name[ind], data = data.tmp)
  }
  return(tasks)
}
