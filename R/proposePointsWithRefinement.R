proposePointsWithRefinement = function(opt.state) {
  # get result for un-refined optimization
  res = proposePointsByInfillOptimization(opt.state)

  # get various objects from the opt state
  control = getOptStateOptProblem(opt.state)$control
  opt.problem = getOptStateOptProblem(opt.state)
  fun = getOptProblemFun(opt.problem)
  design = getOptProblemDesign(opt.problem)
  learner = getOptProblemLearner(opt.problem)
  par.set = getOptProblemParSet(opt.problem)

  # this is usually set in mbo/mboTemplate
  # TODO: the structure here is not good.
  refinement.control = control$refinement$control
  refinement.control$noisy = isNoisy(fun)
  refinement.control$minimize = shouldBeMinimized(fun)

  # construct the refinement learner
  refinement.learner = control$refinement$learner
  refinement.learner = checkLearner(refinement.learner, par.set, refinement.control, fun) # TODO: checkLearner should not set stuff
  factor.values = as.list(res$prop.points[1L, ])
  factor.values = factor.values[!vlapply(factor.values, is.numeric)]
  refinement.learner = makeRefinementWrapper(refinement.learner, factor.values = factor.values)

  # this is usually called in mbo/mboTemplate
  refinement.control = checkStuff(fun, par.set, design, refinement.learner, refinement.control) # TODO: checkStuff should not set stuff
  refinement.control$infill.crit = initCrit(refinement.control$infill.crit, fun, design, refinement.learner, refinement.control) # TODO: initCrit should not be necessary

  # construct new opt.state
  refinement.state = list2env(as.list(opt.state))
  refinement.state$opt.problem = list2env(as.list(opt.state$opt.problem))
  refinement.state$opt.problem$learner = refinement.learner
  refinement.state$opt.problem$control = refinement.control
  refinement.state$refinement = NULL # avoid recursion
  refinement.state$models = NULL # disable caching

  # propose with refinement
  new.res = proposePoints.OptState(refinement.state)

  # patch old object with
  res$prop.points = insert(res$prop.points, new.res$prop.points)
  res$propose.time = res$propose.time + new.res$propose.time
  res$prop.type = paste0(res$prop.type, "/", new.res$prop.type)

  res
}
