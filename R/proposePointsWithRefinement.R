proposePointsWithRefinement = function(opt.state) {
  res = proposePointsByInfillOptimization(opt.state)

  factor.values = as.list(res$prop.points[1L, ])
  factor.values = factor.values[!vlapply(factor.values, is.numeric)]
  control = getOptStateOptProblem(opt.state)$control

  refinement.control = control$refinement$control
  refinement.learner = control$refinement$learner
  refinement.state = list2env(as.list(opt.state))
  refinement.state$opt.problem = list2env(as.list(opt.state$opt.problem))

  fun = getOptProblemFun(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  fun = getOptProblemFun(opt.problem)
  design = getOptProblemDesign(opt.problem)
  learner = getOptProblemLearner(opt.problem)
  par.set = getOptProblemParSet(opt.problem)

  refinement.control$noisy = isNoisy(fun)
  refinement.control$minimize = shouldBeMinimized(fun)
  refinement.learner = checkLearner(refinement.learner, par.set, refinement.control, fun)
  refinement.learner = makeRefinementWrapper(refinement.learner, factor.values = factor.values)
  refinement.control = checkStuff(fun, par.set, design, refinement.learner, refinement.control)
  refinement.control$infill.crit = initCrit(refinement.control$infill.crit, fun, design, refinement.learner, refinement.control)

  refinement.state$opt.problem$learner = refinement.learner
  refinement.state$opt.problem$control = refinement.control
  refinement.state$refinement = NULL
  refinement.state$models = NULL
  new.res = proposePoints.OptState(refinement.state)

  res$prop.points = new.res$prop.points
  res$propose.time = res$propose.time + new.res$propose.time
  res$prop.type = paste0(res$prop.type, "_refined_", new.res$prop.type)
  res
}
