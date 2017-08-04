initOptProblem = function(fun, design, learner, control, show.info, more.args) {
  assertClass(fun, "smoof_function")
  par.set = getParamSet(fun)
  n.params = sum(getParamLengths(par.set))
  control$noisy = isNoisy(fun)
  control$minimize = shouldBeMinimized(fun)
  assertFlag(show.info)
  if (is.null(design))
    design = generateDesign(n.params * 4L, par.set, fun = lhs::maximinLHS)
  else
    assertDataFrame(design, min.rows = 1L, min.cols = 1L)
  learner = checkLearner(learner, control, fun)
  control = checkStuff(fun, design, learner, control)
  control$infill.crit = initCrit(control$infill.crit, fun, design, learner, control)

  loadPackages(control)

  # generate an OptProblem which gathers all necessary information to define the optimization problem in one environment.
  makeOptProblem(
    fun = fun,
    design = design,
    learner = learner,
    control = control,
    show.info = show.info,
    more.args = more.args)
}
