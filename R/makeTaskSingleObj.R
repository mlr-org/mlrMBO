# Generates single objective task.
#
# - only uses x and y columns of optpath
# - converts data types for regr model
# - imputes features

# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{\link[mlr]{SupervisedTask}}]
makeTaskSingleObj = function(opt.path, opt.problem) {
  control = getOptProblemControl(opt.problem)
  data = convertOptPathToDf(opt.path, control)
  data$dob = data$eol = NULL
  y.name = control$y.name

  # user requested handling of NA y values in Opt-Path
  if (control$schedule.method == "asyn" && anyNA(data[[y.name]])) {
    impute.y = switch(control$asyn.impute.method,
      mean = asynImputeMean,
      min = asynImputeMin,
      max = asynImputeMax)
    print(data)
    new.y = impute.y(opt.problem, data = data, y.name = y.name)
  }

  # user selected to (log)-transform the y-column
  trafo.y.fun = control$trafo.y.fun
  if (!is.null(trafo.y.fun)) {
    
    # We stop the process if negative values occur
    data[[y.name]] = trafo.y.fun(data[[y.name]])
  }

  makeRegrTask(target = control$y.name, data = data)
}
