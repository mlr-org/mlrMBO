# directly call an optimizer on an infill crit and output 1 ore more proposed points
# input: models (list), fitted to 1 or more objectives
# output:
#   prop.points [data-frame]  :
#   errors.models [character] :
proposePointsByInfillOptimization = function(models, par.set, control, opt.path, iter, models.unlist, ...) {
  n = control$propose.points
  # m = control$number.of.targets
  # generate a few random points if ANY model failed
  isfail = vlapply(models, isFailureModel)
  crit.vals = rep(NA_real_, n)
  errors.model = NA_character_

  if (any(isfail)) {
    # if error in any model, return first msg
    errors.model = getFailureModelMsg(models[[which.first(isfail)]])
    prop.points = generateDesign(n, par.set, randomLHS)
    prop.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
  } else {
    if (models.unlist)
      models = models[[1L]]
    design = convertOptPathToDf(par.set, opt.path, control)
    infill.crit.fun = getInfillCritFunction(control$infill.crit)
    infill.opt.fun = getInfillOptFunction(control$infill.opt)
    prop.points = infill.opt.fun(infill.crit.fun, models, control, par.set, opt.path, design, iter, ...)
  }
  # mspot is a bit special, we have multiple crit.val
  if (control$multicrit.method == "mspot") {
    crit.vals = asMatrixCols(lapply(models, infill.crit.fun, points = prop.points,
        control = control, par.set = par.set, design = design, iter = iter, ...))
  } else {
    crit.vals = infill.crit.fun(prop.points, models, control, par.set, design, iter, ...)
  }
  if (!is.matrix(crit.vals))
    crit.vals = matrix(crit.vals, ncol = 1L)
  return(list(prop.points = prop.points, crit.vals = crit.vals, errors.model = errors.model))
}

