# Generates single obj task
#
# - only uses x and y columns of optpath
# - converts data types for regr model
# - imputes features
#
makeMBOSingleObjTask = function(par.set, opt.path, control) {
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  des = as.data.frame(opt.path)[, c(pids, control$y.name)]
  des = convertDataFrameCols(des, ints.as.num = TRUE, logicals.as.factor = TRUE)
  # FIXME: Use mlr here!
  des = imputeFeatures(des, par.set, control)
  makeRegrTask(target = control$y.name, data = des)
}
