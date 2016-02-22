# random Search

randomSearch = function(fun, design = NULL, control, show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {

  assertClass(fun, "smoof_function")
  par.set = smoof::getParamSet(fun)
  control$noisy = isNoisy(fun)
  control$minimize = shouldBeMinimized(fun)

  #wrap fun for early termination
  force(fun)
  fun2 = function (...) {
    if (getOptStateTermination(opt.state)$term) {
      NA_real_
    } else {  
      fun(...)
    }
  }
  class(fun2) = c("smoof_wrapped_function")

  opt.problem = makeOptProblem(
    fun = fun,
    par.set = par.set,
    design = design,
    learner = NULL,
    control = control,
    show.info = show.info,
    more.args = more.args)

  opt.state = makeOptState(opt.problem)

  #wrap fun for early termination
  force(fun)
  fun2 = function (...) {
    if (getOptStateTermination(opt.state)$term) {
      NA_real_
    } else {  
      fun(...)
    }
  }
  class(fun2) = c("smoof_wrapped_function")

  evalMBODesign.OptState(opt.state)
  setOptStateLoop(opt.state)
  
  n = min(control$iters, 10000L)
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
