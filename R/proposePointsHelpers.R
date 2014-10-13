# some functions call proposePointByInfillOptimization multiple times
# this helpers joins these results to a single result
joinProposedPoints = function(props) {
  list(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE))
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
    if (control$number.of.targets > 1L && control$multicrit.method == "mspot")
      prop$crit.vals = matrix(rep(NA_real_), nrow = npoints, ncol = control$number.of.targets)
    else
      prop$crit.vals = matrix(rep(NA_real_, npoints), ncol = 1L)
    prop$errors.model = getFailureModelMsg(models[[which.first(isfail)]])
  }
  return(list(ok = all(!isfail), prop = prop))
}


# create control objects with random lamda values for parallel LCB multi-point
createRandomLCBControls = function(control, crit, user.lambda = FALSE) {
  lambdas = rexp(control$propose.points)
  controls = lapply(lambdas, function(lambda) {
    ctrl = control;
    ctrl$propose.points = 1L;
    ctrl$infill.crit = crit
    if (!user.lambda)
      ctrl$infill.crit.lcb.lambda = lambda
    return(ctrl)
  })
  list(lambdas = lambdas, controls = controls)
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

