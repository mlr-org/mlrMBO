proposePointsInfillDistributed = function(opt.state, ...) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)
  models = getOptStateModels(opt.state)$models
  models = if (inherits(models, "WrappedModel")) list(models) else models
  opt.path = getOptStateOptPath(opt.state)
  iter = getOptStateLoop(opt.state)
  n = control$propose.points
  
  infill.crit.fun = getInfillCritFunction(control$infill.crit)
  infill.crit.fun.max = function(...) -1 * infill.crit.fun(...)
  infill.opt.fun = getInfillOptFunction(control$infill.opt)

  st = system.time({
    prop.points = lapply(list(infill.crit.fun, infill.crit.fun.max), infill.opt.fun, models = models, control = control, par.set = par.set, opt.path = opt.path, design = design, iter = iter, ...)

    prop.points.converted = lapply(prop.points, convertDataFrameCols, ints.as.num = TRUE, logicals.as.factor = TRUE)

    prop.points.converted = do.call(rbind, prop.points.converted)
    
    crit.vals = infill.crit.fun.max(prop.points.converted, models, control, par.set, design, iter, ...)

    f = function(x) {
      x = convertListOfRowsToDataFrame(list(x))
      infill.crit.fun.max(x, models, control, par.set, design, iter, ...)
    }
    f.max = max(crit.vals)
    f.min = min(crit.vals)
  })

  st2 = system.time({
    prop.points = rejSamp(f = f, n = n, par.set = par.set, f.max = f.max, f.min = f.min)
  })

  prop.points = convertListOfRowsToDataFrame(prop.points)
  prop.points.converted = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
  crit.vals = infill.crit.fun(prop.points.converted, models, control, par.set, design, iter, ...)

  # calcullate proposal time. min max finding goes to first. rest devided.
  propose.time = rep(st2[3L] / n, n)
  propose.time[1L] = propose.time[1L] + st[3L]


  list(
    prop.points = prop.points,
    crit.vals = matrix(crit.vals, ncol = 1L),
    propose.time = propose.time,
    errors.model = rep.int(NA_character_, n)
  )
}
