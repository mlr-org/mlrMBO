# directly call an optimizer on an infill crit and output 1 or more proposed points
#
# input: models               : EITHER a simgle model or a list of models, depending on the method
#
# output:
#   prop.points [data.frame]  : the proposed points, 1 per row, with n rows
#   crit.vals [matrix(n, k)]  : crit vals for proposed points. rows = points
#                               for some methods, we have a cv for each objective.
#                               in this case k > 1 and typically k = number.of.targets
#   errors.models [character] : model errors, resulting in randomly proposed points.
#                               length is one string PER PROPOSED POINT, not per element of <models>
#                               NA if the model was Ok, or the (first) error message if some model crashed
  proposePointsByInfillOptimization = function(models, par.set, control, opt.path, iter) {
  n = control$propose.points
  # ensure we have a list
  models.list = if (inherits(models, "WrappedModel")) list(models) else models
  # generate a few random points if ANY model failed
  isfail = vlapply(models.list, isFailureModel)
  errors.model = NA_character_
  if (any(isfail)) {
    # if error in any model, return first msg
    errors.model = getFailureModelMsg(models.list[[which.first(isfail)]])
    prop.points = generateDesign(n, par.set, randomLHS)
    prop.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    crit.vals = matrix(rep(NA_real_, n), ncol = 1L)
  } else {
    design = convertOptPathToDf(par.set, opt.path, control)
    infill.crit.fun = getInfillCritFunction(control$infill.crit)
    infill.opt.fun = getInfillOptFunction(control$infill.opt)
    prop.points = infill.opt.fun(infill.crit.fun, models, control, par.set, opt.path, design, iter)
    # mspot is a bit special, we have multiple crit.val
    if (control$multicrit.method == "mspot") {
      crit.vals = asMatrixCols(lapply(models, infill.crit.fun, points = prop.points,
          control = control, par.set = par.set, design = design, iter = iter))
    } else {
      crit.vals = infill.crit.fun(prop.points, models, control, par.set, design, iter)
      crit.vals = matrix(crit.vals, ncol = 1L)
    }
  }
  return(list(prop.points = prop.points, crit.vals = crit.vals, errors.model = errors.model))
}

