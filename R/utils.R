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
    assertClass(learner, "Learner")
  }
  # so we dont run into problems with focus search et al
  learner$fix.factors.prediction = TRUE
  return(learner)
}

# load required extra packages
loadPackages = function(control) {
  if (control$infill.opt == "cmaes")
    requirePackages("cmaes", why = "proposePoints")
  if (control$number.of.targets == 1L && control$propose.points > 1L && control$multipoint.method == "multicrit")
    requirePackages("emoa", why = "proposePoints")
}

evalProposedPoints.OptState = function(opt.state, prop) {
  opt.problem = getOptStateOptProblem(opt.state)
  par.set = getOptProblemParSet(opt.problem)
  extras = getExtras(
    n = nrow(prop$prop.points),
    prop = prop,
    train.time = getOptStateModels(opt.state)$train.time,
    control = getOptProblemControl(opt.problem)
  )
  xs = dfRowsToList(prop$prop.points, par.set)
  xs = lapply(xs, repairPoint, par.set = par.set)

  evalTargetFun.OptState(opt.state, xs = xs, extras = extras, xs.schedule.info = makeScheduleInfo(prop, opt.state))
}

# for Parego: calculate all integer vectors of length k with sum n
combWithSum = function(n, k) {
  fun = function(n, k) {
    if (k == 1L)
      list(n)
    else
      unlist(lapply(0:n, function(i) Map(c, i, fun(n - i, k - 1L))),
        recursive = FALSE)
  }
  matrix(unlist(fun(n, k)), ncol = k, byrow = TRUE)
}

getFileBackupName = function(fn) {
  file.path(dirname(fn), sprintf(".~%s", basename(fn)))
}
