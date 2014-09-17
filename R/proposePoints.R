# Propose infill points - simple dispatcher to real methods
#
# input:
#   tasks [list]              : list of gnerated tasks
#   models [list]             : list of models, fitted to tasks
#
# output:
#   prop.points [data.frame]  : the proposed points, 1 per row, with n rows
#   crit.vals [matrix(n, k)]  : crit vals for proposed points. rows = points
#                               for some methods, we have a cv for each objective.
#                               in this case k > 1 and typically k = number.of.targets
#   errors.models [character] : model errors, resulting in randomly proposed points.
#                               length is one string PER PROPOSED POINT, not per element of <models>
#                               NA if the model was Ok, or the (first) error message if some model crashed

proposePoints = function(tasks, models, par.set, control, opt.path, iter) {
  n = control$propose.points
  m = control$number.of.targets
  if (m == 1L) {
    if (n == 1L) {
      res = proposePointsByInfillOptimization(models[[1L]], par.set, control, opt.path, iter)
    } else {
      if (control$multipoint.method == "lcb")
        res = proposePointsParallelLCB(models, par.set, control, opt.path, iter)
      else if (control$multipoint.method == "cl")
        res = proposePointsConstantLiar(models, par.set, control, opt.path, iter)
      else if (control$multipoint.method == "multicrit") {
        res = proposePointsMOIMBO(models, par.set, control, opt.path, iter)
      }
    }
  } else {
      if (control$multicrit.method == "parego") {
        res = proposePointsParEGO(models, par.set, control, opt.path, iter, attr(tasks, "weight.mat"))
      } else if (control$multicrit.method == "mspot") {
        res = proposePointsByInfillOptimization(models, par.set, control, opt.path, iter)
      } else if (control$multicrit.method == "dib") {
        res = proposePointsDIB(models, par.set, control, opt.path, iter)
      }
  }

  if (!is.matrix(res$crit.vals))
    res$crit.vals = matrix(res$crit.vals, ncol = 1L)
  return(res)
}



