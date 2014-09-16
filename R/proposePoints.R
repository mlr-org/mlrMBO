# Propose infill points.
#
# Works for single and multicrit, can propose 1 or multi-points
# - gets infill crit and optimizer, depending on control
# - then runs its
#
# returns:
# - prop.points [data.frame]: the proposed points, with n rows
# - crit.vals [matrix(n, k)]: the crit values at the n points
#   for some methods, we have a cv for each objective. in this case k > 1, typically k = number.of.targets
# - errors.model [character(1)]: NA if the model was Ok, the (first) error message if some model crashed

proposePoints = function(tasks, models, par.set, control, opt.path, iter, ...) {
  n = control$propose.points
  m = control$number.of.targets
  if (m == 1L) {
    if (n == 1L) {
      res = proposePointsByInfillOptimization(models, par.set, control, opt.path, iter, models.unlist = TRUE,...)
    } else {
      if (control$multipoint.method == "lcb")
        res = proposePointsParallelLCB(models, par.set, control, opt.path, iter, ...)
      else if (control$multipoint.method == "cl")
        res = proposePointsConstantLiar(models, par.set, control, opt.path, iter, ...)
      else if (control$multipoint.method == "multicrit") {
        res = proposePointsMOIMBO(models, par.set, control, opt.path, iter, ...)
        # FIXME: this is bad.
        res$errors.model = NA_character_
      }
    }
  } else {
      if (control$multicrit.method == "parego") {
        res = proposePointsParEGO(models, par.set, control, opt.path, iter, attr(tasks, "weight.mat"))
      } else {
        res = proposePointsByInfillOptimization(models, par.set, control, opt.path, iter, models.unlist = FALSE, ...)
      }
  }

  if (!is.matrix(res$crit.vals))
    res$crit.vals = matrix(res$crit.vals, ncol = 1L)
  return(res)
}



