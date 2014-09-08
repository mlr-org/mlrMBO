
# FIXME: For the moment excluded - talk about it
# Random Search for the multicrit optimization of all models
#infillOptMultiCritRandom = function(infill.crit, models, control, par.set, opt.path, design, ...) {
#  requirePackages("emoa")
#  
#  newdesign = generateDesign(control$infill.opt.multicrit.randomsearch.points, par.set,
#    randomLHS)
#  
#  FUN.VALUE = rep(0, control$infill.opt.multicrit.randomsearch.points)
#  ys = vapply(models, infill.crit, FUN.VALUE = FUN.VALUE, points = newdesign, control = control,
#    par.set = par.set, design = design, ...)
#  
#  front.inds = !is_dominated(t(ys))
#  return(list(
#    points = recodeTypes(newdesign[front.inds, , drop = FALSE], par.set),
#    crit.vals = ys[front.inds, , drop = FALSE])
#  )
#}

# NSGA 2
infillOptMultiCritNSGA2 = function(infill.crit, models, control, par.set, opt.path, design, ...) {
  requirePackages("mco")
  
  rep.pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  
  fun.tmp = function(x) {
    newdata = as.data.frame(t(x))
    colnames(newdata) = rep.pids
    vapply(models, infill.crit, FUN.VALUE = 0, points = newdata, control = control,
      par.set = par.set, design = design, ...)
  }
  
  res = nsga2(fun.tmp, idim = getParamNr(par.set, devectorize = TRUE), odim = control$number.of.targets,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = control$infill.opt.nsga2.popsize, generations = control$infill.opt.nsga2.generations,
    cprob = control$infill.opt.nsga2.cprob, cdist = control$infill.opt.nsga2.cdist,
    mprob = control$infill.opt.nsga2.mprob, mdist = control$infill.opt.nsga2.mdist, ...)
  
  points = as.data.frame(res$par)
  colnames(points) = rep.pids
  
  best.inds = selectBestHypervolumePoints(res$value, control, opt.path)
  
  return(points = points[best.inds, , drop = FALSE])
}

# gets a data.frame of candidate points and selects the control$prop.points best points
# concerning hypervolume
# returns the indices of the best points
selectBestHypervolumePoints = function (crit.vals, control, opt.path) {
  n = nrow(crit.vals)
  front.old = t(getOptPathY(opt.path))
  crit.vals = t(crit.vals)
  # FIXME: This only works well for proposing 1 point! For proposing more points
  # at the same time, we have to look at the common hv-contr of this points
  # Idea: Do this greedy?
  hvs = -1 * sapply(seq_col(crit.vals), function(i)
    dominated_hypervolume(cbind(front.old, crit.vals[, i]), ref = control$multicrit.ref.point))
  
  order(hvs)[1:control$propose.points]
}
