# Generates MBO task.
#
# @param design [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Initial design.
# @param y.name [\code{character(1)}]\cr
#   Name of y-column for target values in optimization path.
# @return [\code{\link[mlr]{SupervisedTask}]:
#   List of repaired points.
makeMBOTask = function(design, par.set, control) {
  design$dob = design$eol = design[[control$infill.crit]] = design$error.message = NULL
  design = convertDataFrameCols(design, ints.as.num = TRUE, logicals.as.factor = TRUE)
  # FIXME Use mlr here!
  design = imputeFeatures(design, par.set, control)
  #if (control$rank.trafo)
  #  design[,y.name] = rank(design[,y.name])
  makeRegrTask(target = control$y.name, data = design)
}
