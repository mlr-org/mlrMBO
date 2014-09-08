# Propose infill points.
#
# Works for single and multicrit, can propose 1 or multi-points
# - gets infill crit and optimizer, depending on control
# - then runs its
#
# returns:
# - prop.points [data.frame]: the proposed points, with n rows
# - crit.vals [numeric(n)]: the crit values at the points
# - errors.model [character(1)]: NA if the model was Ok, the (first) error message if some model crashed

proposePoints = function(models, par.set, control, opt.path, ...) {
  n = control$propose.points
  m = control$number.of.targets
  # restructure a bit, if we only got one model
  if (inherits(models, "WrappedModel")) {
    model = models
    models = list(model)
  }
  # generate a few random points if ANY model failed
  isfail = vlapply(models, isFailureModel)
  if (any(isfail)) {
    # if error in any model, return first msg
    errors.model = getFailureModelMsg(models[[which.first(isfail)]])
    prop.points = generateDesign(n, par.set, randomLHS)
    propose.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    crit.vals = rep(NA_real_, n)
  } else {
    errors.model = NA_character_
    #FIXME: shoule we impute features here or not?
    #DH: Must be Done in MLR now.
    design = convertOptPathToDf(par.set, opt.path, control)

    # single crit, single point proposal
    if (m == 1L && n == 1L) {
      # get infill fun + optimizer, then run, do final crit eval for return object
      infill.crit.fun = getInfillCritFunction(control$infill.crit)
      infill.opt.fun = getInfillOptFunction(control$infill.opt)
      prop.points = infill.opt.fun(infill.crit.fun, model, control, par.set, opt.path, design, ...)
      crit.vals = infill.crit.fun(prop.points, model, control, par.set, design, ...)
   # multi point proposal
    } else {
      # get optimizer and run
      multipoint.infill.opt.fun = getMultipointInfillOptFunction(control$multipoint.method)
      prop.design = multipoint.infill.opt.fun(model, control, par.set, opt.path, design, ...)
      prop.points = prop.design$prop.points
      crit.vals = prop.design$crit.vals
    }
  }
  return(list(prop.points = prop.points, crit.vals = crit.vals, errors.model = errors.model))
}



