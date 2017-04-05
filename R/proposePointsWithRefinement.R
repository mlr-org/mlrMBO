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

  #subset everything yo the numeric space
  reduced.par.set = filterParamsNumeric(par.set)

  # generate subset design
  factor.values = as.list(res$prop.points[1L, ])
  factor.values = factor.values[!vlapply(factor.values, is.numeric)]

  tasks = getOptStateTasks(opt.state)[[1]]
  data = as.data.table(getTaskData(tasks))
  keep = setdiff(names(data), names(factor.values))
  reduced.design = data[factor.values, keep, on = names(factor.values), nomatch = 0L, with = FALSE]
  refinement.tasks = list(mlr:::changeData(tasks, as.data.frame(reduced.design)))

  # change parset of smoof function
  reduced.function = fun
  attr(reduced.function, "par.set") = reduced.par.set

  # this is usually set in mbo/mboTemplate
  # TODO: the structure here is not good.
  refinement.control = control$refinement$control
  refinement.control$noisy = isNoisy(reduced.function)
  refinement.control$minimize = shouldBeMinimized(reduced.function)


  # construct the refinement learner
  refinement.learner = control$refinement$learner
  refinement.learner = checkLearner(refinement.learner, reduced.par.set, refinement.control, reduced.function) # TODO: checkLearner should not set stuff

  # refinement.learner = makeRefinementWrapper(refinement.learner, factor.values = factor.values)

  # this is usually called in mbo/mboTemplate
  refinement.control = checkStuff(reduced.function, reduced.par.set, reduced.design, refinement.learner, refinement.control) # TODO: checkStuff should not set stuff
  refinement.control$infill.crit = initCrit(refinement.control$infill.crit, reduced.function, reduced.design, refinement.learner, refinement.control) # TODO: initCrit should not be necessary

  # construct new opt.state
  refinement.state = list2env(as.list(opt.state))
  refinement.state$opt.problem = list2env(as.list(opt.state$opt.problem))
  refinement.state$opt.problem$learner = refinement.learner
  refinement.state$opt.problem$control = refinement.control
  refinement.state$opt.problem$design = reduced.design
  refinement.state$opt.problem$fun = reduced.function
  refinement.state$refinement = NULL # avoid recursion
  refinement.state$models = NULL # disable caching
  setOptStateTasks(refinement.state, refinement.tasks)

  # propose with refinement
  new.res = proposePoints.OptState(refinement.state)

  # patch old object with
  res$prop.points = insert(res$prop.points, new.res$prop.points)
  res$propose.time = res$propose.time + new.res$propose.time
  res$prop.type = paste0(res$prop.type, "/", new.res$prop.type)

  res
}
