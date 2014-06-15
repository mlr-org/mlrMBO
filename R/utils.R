# show info without if-statement
showInfo = function(show.info, ...) {
  if (show.info)
    messagef(...)
}

# check and create default learner
checkLearner = function(learner, par.set, control, ...) {
  if (missing(learner)) {
    if (hasDiscrete(par.set))
      learner = makeLearner("regr.randomForest", predict.type = "se")
    else
      learner = makeLearner("regr.km", covtype = "matern5_2", predict.type = "se",
        nugget.estim = control$noisy, ...)
  } else {
    checkArg(learner, "Learner")
  }
  # FIXME: I am unsure wether we should do this, but otherwise RF sucks
  # if it is a good idea it is not not general enuff
  if (inherits(learner, "regr.randomForest"))
    learner = setHyperPars(learner, fix.factors = TRUE)
  return(learner)
}

# load required extra packages
loadPackages = function(control) {
  if (control$infill.opt == "cmaes")
    requirePackages("cmaes", "proposePoints")
  if (control$multipoint.method == "multicrit")
    requirePackages("emoa", "proposePoints")
}

# to list + repair + eval
evalProposedPoints = function(loop, prop.points, par.set, opt.path, control,
  fun, learner, show.info, oldopts, more.args, extras) {

  xs = dfRowsToList(prop.points, par.set)
  xs = lapply(xs, repairPoint, par.set = par.set)
  evalTargetFun(fun, par.set, loop, xs, opt.path, control, show.info, oldopts, more.args, extras)
}

