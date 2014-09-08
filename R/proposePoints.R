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
  # make sure models.list is always a LIST, model can be ONE model or list of, depends on case we are in
  models.list = ensureVector(models, 1L, cl = "WrappedModel")
  # generate a few random points if ANY model failed
  isfail = vlapply(models.list, isFailureModel)
  if (any(isfail)) {
    # if error in any model, return first msg
    errors.model = getFailureModelMsg(models.list[[which.first(isfail)]])
    prop.points = generateDesign(n, par.set, randomLHS)
    propose.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    crit.vals = rep(NA_real_, n)
  } else {
    errors.model = NA_character_
    #FIXME: shoule we impute features here or not?
    #DH: Must be Done in MLR now.
    design = convertOptPathToDf(par.set, opt.path, control)

    # single crit, multipoint, a bit special case
    if (m == 1L && n > 1L) {
      # get optimizer and run
      multipoint.infill.opt.fun = getMultipointInfillOptFunction(control$multipoint.method)
      prop.design = multipoint.infill.opt.fun(models, control, par.set, opt.path, design, ...)
      prop.points = prop.design$prop.points
      crit.vals = prop.design$crit.vals
    } else {
      infill.crit.fun = getInfillCritFunction(control$infill.crit)
      infill.opt.fun = getInfillOptFunction(control$infill.opt)
      prop.points = infill.opt.fun(infill.crit.fun, models, control, par.set, opt.path, design, ...)
      # mspot is a bit special, we have multiple crit.vals
      if (control$multicrit.method == "mspot") {
        # FIXME clean up
        crit.vals = vapply(models, infill.crit.fun, FUN.VALUE = rep(0, nrow(prop.points)),
          points = prop.points, control = control, par.set = par.set, design = design, ...)
        if (is.vector(crit.vals)) crit.vals = matrix(crit.vals, nrow = 1)
      }
      else {
        crit.vals = infill.crit.fun(prop.points, models, control, par.set, design, ...)
      }
    }
  }
  return(list(prop.points = prop.points, crit.vals = crit.vals, errors.model = errors.model))
}



