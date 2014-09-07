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
