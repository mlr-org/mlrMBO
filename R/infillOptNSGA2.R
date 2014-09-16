

# NSGA 2
infillOptMultiCritNSGA2 = function(infill.crit, models, control, par.set, opt.path, design, iter, ...) {

  rep.pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  m = control$number.of.targets
  control2 = control
  fun.tmp = function(x) {
    newdata = as.data.frame(t(x))
    colnames(newdata) = rep.pids
    vnapply(1:m, function(i) {
      # we need to make sure mininimize in control is a scalar, so we can multiply it in infill crits...
      control2$minimize = control$minimize[i]
      infill.crit(points = newdata, model = models[[i]], control = control2,
        par.set = par.set, design = design, iter = iter, ...)
    })
  }

  res = nsga2(fun.tmp, idim = getParamNr(par.set, devectorize = TRUE), odim = control$number.of.targets,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = control$infill.opt.nsga2.popsize, generations = control$infill.opt.nsga2.generations,
    cprob = control$infill.opt.nsga2.cprob, cdist = control$infill.opt.nsga2.cdist,
    mprob = control$infill.opt.nsga2.mprob, mdist = control$infill.opt.nsga2.mdist, ...)

  points = as.data.frame(res$par)
  colnames(points) = rep.pids

  best.inds = selectBestHypervolumePoints(res$value, control, opt.path, design)

  return(points = points[best.inds, , drop = FALSE])
}

# gets a data.frame of candidate points and selects the control$prop.points best points
# concerning hypervolume
# returns the indices of the best points
selectBestHypervolumePoints = function (crit.vals, control, opt.path, design) {
  n = nrow(crit.vals)
  front.old = t(getOptPathY(opt.path))
  crit.vals = t(crit.vals)
  # FIXME: This only works well for proposing 1 point! For proposing more points
  # at the same time, we have to look at the common hv-contr of this points
  # Idea: Do this greedy?
  ref.point = getMultiCritRefPoint(control, design)
  hvs = -1 * sapply(seq_col(crit.vals), function(i)
    dominated_hypervolume(cbind(front.old, crit.vals[, i]), ref = ref.point))

  order(hvs)[1:control$propose.points]
}
