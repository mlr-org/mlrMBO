# Propose infill points.
#
# Can propose 1 or more points
# - gets infill crit and optimizer, depending on control
# - then runs its
#
# returns:
# - the proposed points as a data.frame with n rows
# - the crit values at the points
# - NA if the model was Ok, the error message if it crashed

# FIXME: We should unify this with other function
# FIXME: where do we get the LCB lambda value from? is this a settable option in control?
# FIXME: do we allow to sample muliple lambda for multipoint?
proposePointsSMS = function(models, par.set, control, opt.path, ...) {
  n = control$propose.points
  # generate a few random points if model failed
  # FIXME: What if only part of the models fail? Optimize the remaining? Random Points?
  if (any(sapply(models, isFailureModel))) {
    error.model = getFailureModelMsg(model)
    prop.points = generateDesign(n, par.set, randomLHS)
    propose.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    crit.vals = rep(NA_real_, n)
  } else {
    error.model = NA_character_
    design = convertOptPathToDf(par.set, opt.path, control)
    infill.crit.fun = infillCritSMS
    infill.opt.fun = getInfillOptFunction(control$infill.opt)
    prop.points = infill.opt.fun(infill.crit.fun, models, control, par.set, opt.path, design, ...)
    crit.vals = infill.crit.fun(prop.points, models, control, par.set, design, ...)
  }
  return(list(prop.points = prop.points, crit.vals = crit.vals, error.model = error.model))
}

