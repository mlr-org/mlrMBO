# some functions call proposePointByInfillOptimization multiple times
# this helpers joins these results to a single result
joinProposedPoints = function(props) {
  makeProposal(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    propose.time = do.call(c, extractSubList(props, "propose.time", simplify = FALSE)),
    prop.type = do.call(c, extractSubList(props, "prop.type", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE)),
    crit.components = data.table::rbindlist(extractSubList(props, "crit.components", simplify = FALSE), fill = TRUE)
  )
}

# generate a few random points if ANY model failed
checkFailedModels = function(models, par.set, npoints, control) {
  isfail = vlapply(models, isFailureModel)
  if (any(isfail)) {
    # if error in any model, return first msg
    prop.points = generateDesign(npoints, par.set, randomLHS)
    prop.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    prop = makeProposal(
      control = control,
      prop.points = prop.points,
      prop.type = "random_error",
      errors.model = getFailureModelMsg(models[[which.first(isfail)]])
    )
  } else {
    prop = NULL
  }

  return(list(ok = all(!isfail), prop = prop))
}


# create control objects with random lamda values for parallel cb multi-point
# @arg crit: MBOInfillCrits
createSinglePointControls = function(opt.problem, crits) {

  control = getOptProblemControl(opt.problem)
  design = getOptProblemDesign(opt.problem)
  learner = getOptProblemLearner(opt.problem)
  fun = getOptProblemFun(opt.problem)

  lapply(crits, function(crit) {
    ctrl = control;
    crit = initCrit(crit, fun, design, learner, control)
    ctrl$propose.points = 1L
    ctrl$infill.crit = crit
    return(ctrl)
  })
}

# perform a deep copy of an opt.path object
# so we can store (temporary) stuff in it, without changing the real opt.path
# needed in CL and DIB multi-point
deepCopyOptPath = function(op) {
  UseMethod("deepCopyOptPath")
}

deepCopyOptPath.OptPathNg = function(op) {
  op$clone(deep = TRUE)
}


deepCopyOptPath.OptPath = function(op) {
  op2 = op
  op2$env = new.env()
  op2$env$path = op$env$path
  op2$env$dob = op$env$dob
  op2$env$eol = op$env$eol
  return(op2)
}
