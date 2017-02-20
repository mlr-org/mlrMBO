# some functions call proposePointByInfillOptimization multiple times
# this helpers joins these results to a single result
joinProposedPoints = function(props) {
  list(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    propose.time = do.call(c, extractSubList(props, "propose.time", simplify = FALSE)),
    prop.type = do.call(c, extractSubList(props, "prop.type", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE)),
    crit.components = do.call(rbind, extractSubList(props, "crit.components", simplify = FALSE))
  )
}

# generate a few random points if ANY model failed
checkFailedModels = function(models, par.set, npoints, control) {
  isfail = vlapply(models, isFailureModel)
  prop = NULL
  if (any(isfail)) {
    # if error in any model, return first msg
    prop.points = generateDesign(npoints, par.set, randomLHS)
    prop$prop.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    # mspot is the special kid, that needs multiple crit vals
    if (control$n.objectives > 1L && control$multiobj.method == "mspot")
      prop$crit.vals = matrix(rep(NA_real_), nrow = npoints, ncol = control$n.objectives + 1)
    else
      prop$crit.vals = matrix(rep(NA_real_, npoints), ncol = 1L)
    prop$errors.model = getFailureModelMsg(models[[which.first(isfail)]])
    prop$propose.time = rep(NA_real_, npoints)
  }
  return(list(ok = all(!isfail), prop = prop))
}


# create control objects with random lamda values for parallel cb multi-point
# @arg crit: MBOInfillCrit
# @arg crit.pars: list of length propose.points.
#   Each list item contains a list with the arguments the infill crit should be initialized
createSinglePointControls = function(control, crit, crit.pars = NULL) {
  if (is.null(crit.pars)) {
    crit.pars = replicate(control$propose.points, list())
  }
  assertList(crit.pars, len = control$propose.points)
  lapply(crit.pars, function(crit.par) {
    ctrl = control;
    ctrl$propose.points = 1L
    ctrl$infill.crit = do.call(crit, crit.par)
    return(ctrl)
  })
}

# perform a deep copy of an opt.path object
# so we can store (temporary) stuff in it, without changing the real opt.path
# needed in CL and DIB multipoint
deepCopyOptPath = function(op) {
  op2 = op
  op2$env = new.env()
  op2$env$path = op$env$path
  op2$env$dob = op$env$dob
  op2$env$eol = op$env$eol
  return(op2)
}
