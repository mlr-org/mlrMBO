# Generates MBO task.

makeMBOTask = function(design, par.set, control) {
  data = convertOptPathToDf(par.set, opt.path, control, impute.feats = TRUE)
  makeRegrTask(target = control$y.name, data = data)
}
