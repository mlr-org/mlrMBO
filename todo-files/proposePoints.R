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

proposePoints = function(model, par.set, control, opt.path, ...) {
  n = control$propose.points
  # generate a few random points if model failed
  if (isFailureModel(model)) {
    errors.model = getFailureModelMsg(model)
    prop.points = generateDesign(n, par.set, randomLHS)
    propose.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    crit.vals = rep(NA_real_, n)
  } else {
    errors.model = NA_character_
    #FIXME: shoule we impute features here or not?
    #DH: Must be Done in MLR now.
    design = convertOptPathToDf(par.set, opt.path, control)

    if (n == 1L) {
      # single point proposal
      # get infill fun + optimizer, then run, do final crit eval for return object
      infill.crit.fun = getInfillCritFunction(control$infill.crit)
      infill.opt.fun = getInfillOptFunction(control$infill.opt)
      prop.points = infill.opt.fun(infill.crit.fun, model, control, par.set, opt.path, design, ...)
      crit.vals = infill.crit.fun(prop.points, model, control, par.set, design, ...)
    } else {
      # multi point proposal
      # get optimizer and run
      multipoint.infill.opt.fun = getMultipointInfillOptFunction(control$multipoint.method)
      prop.design = multipoint.infill.opt.fun(model, control, par.set, opt.path, design, ...)
      prop.points = prop.design$prop.points
      crit.vals = prop.design$crit.vals
    }
  }
  return(list(prop.points = prop.points, crit.vals = crit.vals, errors.model = errors.model))
}


# FIXME: We should unify this with other function
# FIXME: where do we get the LCB lambda value from? is this a settable option in control?
# FIXME: do we allow to sample muliple lambda for multipoint?
proposePointsSMS = function(models, par.set, control, opt.path, ...) {
  n = control$propose.points
  # generate a few random points if model failed
  # FIXME: What if only part of the models fail? Optimize the remaining? Random Points?
  isfail = vlapply(models, isFailureModel)
  if (any(isfail)) {
    # if error in any model, return first msg
    errors.model = getFailureModelMsg(models[which.first(isfail)])
    prop.points = generateDesign(n, par.set, randomLHS)
    propose.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    crit.vals = rep(NA_real_, n)
  } else {
    errors.model = NA_character_
    #FIXME: shoule we impute features here or not?
    #DH: Must be Done in MLR now.
    design = convertOptPathToDf(par.set, opt.path, control)
    infill.crit.fun = getInfillCritFunction(control$infill.crit)
    # FIXME: Add more multicrit optimizer and copy the single-obj structure
    infill.opt.fun = getInfillOptFunction(control$infill.opt)
    prop.points = infill.opt.fun(infill.crit.fun, models, control, par.set, opt.path, design, ...)
    crit.vals = infill.crit.fun(prop.points, models, control, par.set, design, ...)


    candidates = infill.opt.fun(infill.crit.fun, models, control, par.set, opt.path, design, ...)
    # FIXME: What if less candidates than prop.points?
    # FIXME: Add more selection criterias
    propose.inds = selectBestHypervolumePoints(candidates$crit.vals, control, opt.path)
    prop.points = candidates$points[propose.inds, ]
    crit.vals = candidates$crit.vals[propose.inds, ]
  }
  return(list(prop.points = prop.points, crit.vals = crit.vals, errors.model = errors.model))
}



