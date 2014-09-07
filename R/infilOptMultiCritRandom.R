# Random Search for the multicrit optimization of all models
infillOptMultiCritRandom = function(infill.crit, models, control, par.set, opt.path, design, ...) {
  requirePackages("emoa")
  
  # FIXME introduce owm params, don't use infill.opt.focussearch ones
  newdesign = generateDesign(control$infill.opt.multicrit.randomsearch.points, par.set,
    randomLHS)
  
  FUN.VALUE = rep(0, control$infill.opt.focussearch.points)
  ys = vapply(models, infill.crit, FUN.VALUE = FUN.VALUE, points = newdesign, control = control,
    par.set = par.set, design = design, ...)
  
  front.inds = !is_dominated(t(ys))
  return(list(
    points = recodeTypes(newdesign[front.inds, , drop = FALSE], par.set),
    crit.vals = ys[front.inds, , drop = FALSE])
  )
}

# NSGA 2
infillOptMultiCritNSGA2 = function(infill.crit, models, control, par.set, opt.path, design, ...) {
  requirePackages("mco")
  
  res = nsga2(infill.crit, idim = getParamNr(par.set, devectorize = TRUE), odim = control$number.of.targets,
    control$infill.opt.nsga2.popsize, control$infill.opt.nsga2.generations,
    control$infill.opt.nsga2.cprob, control$infill.opt.nsga2.cdist,
    control$infill.opt.nsga2.mprob, control$infill.opt.nsga2.mdist)
  return(list(
    points = res$par,
    crit.vals = res$value)
  )
}