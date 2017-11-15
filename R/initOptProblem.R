initOptProblem = function(fun, design, learner, control, show.info, more.args) {
  assertClass(fun, "smoof_function")

  par.set = getParamSet(fun)

  assertDataFrame(design, min.rows = 1L, min.cols = 1L, null.ok = TRUE)
  if (!is.null(design)) {
    assertSubset(getParamIds(par.set, repeated = TRUE, with.nr = TRUE), names(design)) 
    checkInitDesign(design, par.set) 
  }

  learner = checkLearner(learner, control, fun)

  assertClass(control, "MBOControl")

  n.params = sum(getParamLengths(par.set))
  control$noisy = isNoisy(fun)
  control$minimize = shouldBeMinimized(fun)
  control = checkStuff(fun, design, learner, control)
  control$infill.crit = initCrit(control$infill.crit, fun, design, learner, control)

  loadPackages(control)

  # generate an OptProblem which gathers all necessary information to define the optimization problem in one environment.
  makeOptProblem(
    fun = fun,
    design = design,
    learner = learner,
    control = control,
    show.info = assertFlag(show.info),
    more.args = assertList(more.args, null.ok = TRUE))
}
