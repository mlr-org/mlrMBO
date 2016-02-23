# random Search

randomSearch = function(fun, design = NULL, control, show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {

  assertClass(fun, "smoof_function")
  par.set = smoof::getParamSet(fun)
  control$noisy = isNoisy(fun)
  control$minimize = shouldBeMinimized(fun)

  opt.problem = makeOptProblem(
    fun = fun,
    par.set = par.set,
    design = design,
    learner = NULL,
    control = control,
    show.info = show.info,
    more.args = more.args)

  opt.state = makeOptState(opt.problem)

  evalMBODesign.OptState(opt.state)
  setOptStateLoop(opt.state)
  
  #roughly estimate time per evaluatio from initial design to account for time budget
  opt.path = getOptStateOptPath(opt.state)
  mean.time.used = mean(getOptPathExecTimes(opt.path))
  time.budget = min(control$time.budget, control$exec.time.budget)
  
  n = min(control$iters, floor(time.budget / mean.time.used))
  prop = list(
    prop.points = generateRandomDesign(par.set = par.set, n = n),
    crit.vals = matrix(rep.int(NA_real_, n), nrow = n, ncol = 1L),
    propose.time = rep.int(NA_real_, n),
    errors.model = rep.int(NA_character_, n)
  )
  setOptStateModels(opt.state, models = list(train.time = 0L))
  evalProposedPoints.OptState(opt.state, prop)
  setOptStateLoop(opt.state)
  mboFinalize2(opt.state)
}
