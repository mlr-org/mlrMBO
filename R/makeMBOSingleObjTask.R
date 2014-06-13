# Generates single obj task
#
# - only uses x and y columns of optpath
# - converts data types for regr model
# - imputes features
#
makeMBOSingleObjTask = function(par.set, opt.path, control) {
  data = convertOptPathToDf(par.set, opt.path, control, impute = TRUE)
  makeRegrTask(target = control$y.name, data = data)
}
