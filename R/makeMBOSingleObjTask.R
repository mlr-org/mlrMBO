# Generates single obj task
#
# - only uses x and y columns of optpath
# - converts data types for regr model
# - imputes features
makeMBOSingleObjTask = function(par.set, opt.path, control) {
  data = convertOptPathToDf(par.set, opt.path, control)
  data$dob = data$eol = NULL
  
  # user selected to (log)-transform the y-column 
  trafo.y.fun = control$trafo.y.fun
  if (!is.null(trafo.y.fun)) {
  	y.name = control$y.name
  	if (any(data[[y.name]] < 0)) {
  		stopf("Transformation '%s' required all inputs to be positive, but negative values occured.")
  	}
  	data[[y.name]] = trafo.y.fun(data[[y.name]])
  }

  makeRegrTask(target = control$y.name, data = data)
}
