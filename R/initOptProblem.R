initOptProblem = function(fun, design, learner, control, show.info, more.args) {
  assertClass(fun, "smoof_function")

  par.set = getParamSet(fun)

  assertDataFrame(design, null.ok = TRUE)
  if (!is.null(design)) {
    assertSubset(getParamIds(par.set, repeated = TRUE, with.nr = TRUE), names(design))  
  }
  

  learner = checkLearner(learner, control, fun)

  assertClass(control, "MBOControl")

  n.params = sum(getParamLengths(par.set))
  control$noisy = isNoisy(fun)
  control$minimize = shouldBeMinimized(fun)
  if (is.null(design))
    design = generateDesign(n.params * 4L, par.set, fun = lhs::maximinLHS)
  else
    assertDataFrame(design, min.rows = 1L, min.cols = 1L)
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
