proposePointsMSPOT = function(opt.state, ...) {

  opt.problem = getOptStateOptProblem(opt.state)
  models = getOptStateModels(opt.state)$models
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  iter = getOptStateLoop(opt.state)

  n = control$propose.points
  ch = checkFailedModels(models, par.set, n, control = control)
  if (!ch$ok)
    return(ch$prop)

  design = convertOptPathToDf(opt.path, control)
  infill.crit.id = getMBOInfillCritId(control$infill.crit)
  infill.opt.fun = getInfillOptFunction(control$infill.opt)

  # store time to propose single point
  secs = measureTime({
    prop.points = infill.opt.fun(control$infill.crit$fun, models, control, par.set, opt.path, design, iter, ...)
  })

  ppoints = prop.points$prop.points
  crit.vals = evalCritFunForMultiObjModels(control$infill.crit$fun, ppoints, models, control,
    par.set, design, iter)
  crit.vals = cbind(crit.vals, prop.points$prop.hv.contrs)
  prop.type = rep(paste0("infill_", infill.crit.id), n)

  return(list(prop.points = ppoints, propose.time = secs, crit.vals = crit.vals, prop.type = prop.type, errors.model = NA_character_))
}
