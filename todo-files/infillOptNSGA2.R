# Multi-objective optimization of multiple infill.crits - one crit per target
# NSGA2 is used to create set of candidate soluation
# Greedy cypervolume contribution selection is used to select prop.point from
# from candidate points
infillOptMultiObjNSGA2 = function(infill.crit, models, control, par.set, opt.path, designs, iter, ...) {

  # build target function for vectorized nsga2 and run it
  rep.pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  m = control$n.objectives
  fun.tmp = function(x) {
    newdata = as.data.frame(x)
    colnames(newdata) = rep.pids
    asMatrixRows(lapply(seq_len(m), function(i) {
      # we need to make sure mininimize in control is a scalar, so we can multiply it in infill crits...
      control$minimize = control$minimize[i]
      control$y.name = control$y.name[i]
      infill.crit(points = newdata, models = models[i], control = control,
        par.set = par.set, designs = designs[i], iter = iter, ...)
    }))
  }
  res = mco::nsga2(fun.tmp, idim = getParamNr(par.set, devectorize = TRUE), odim = control$n.objectives,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set), vectorized = TRUE,
    popsize = control$infill.opt.nsga2.popsize, generations = control$infill.opt.nsga2.generations,
    cprob = control$infill.opt.nsga2.cprob, cdist = control$infill.opt.nsga2.cdist,
    mprob = control$infill.opt.nsga2.mprob, mdist = control$infill.opt.nsga2.mdist)
  points = as.data.frame(res$par)
  colnames(points) = rep.pids

  # now we have nsga2.popsize candidate points and we have to select propose.points
  # do this greedy - select the point with max. hv.contr, add it and select
  # the best point wrt to the new front
  candidate.points = res$par
  # Use the mean/cb response of the model to calculate the hv.contr, not the nsga2-val
  hv.contr.crit = control$mspot.select.crit$fun
  candidate.vals = asMatrixCols(lapply(seq_len(m), function(i) {
    # we need to make sure mininimize in control is a scalar, so we can multiply it in infill crits...
    control$minimize = control$minimize[i]
    control$y.name = control$y.name[i]
    newdata = as.data.frame(res$par)
    colnames(newdata) = rep.pids
    hv.contr.crit(points = newdata, models = models[i], control = control,
      par.set = par.set, designs = designs[i], iter = iter, ...)
  }))

  prop.points = matrix(nrow = 0, ncol = ncol(candidate.points))
  prop.vals = matrix(nrow = 0, ncol = ncol(candidate.vals))
  colnames(prop.vals) = control$y.name
  ys = Map(function(i, y.name) designs[[i]][,y.name], i = seq_along(control$y.name), y.name = control$y.name)
  ys = do.call(cbind, ys)

  ref.point = getMultiObjRefPoint(ys, control)
  prop.hv.contrs = numeric(control$propose.points)
  for (i in seq_len(control$propose.points)) {
    hv.contrs = getHypervolumeContributions(xs = candidate.vals,
      ys = rbind(ys, prop.vals), ref.point = ref.point, minimize = control$minimize)
    best.ind = which.max(hv.contrs)
    prop.hv.contrs[i] = max(hv.contrs)
    # add best to prop.points/vals and remove from candidate.point/vals
    prop.points = rbind(prop.points, candidate.points[best.ind, ])
    candidate.points = candidate.points[-best.ind,, drop = FALSE]
    prop.vals = rbind(prop.vals, candidate.vals[best.ind, ])
    candidate.vals = candidate.vals[-best.ind,, drop = FALSE]
  }

  # FIXME: cleanup - i'm reall unsure how to set the names of prop.points technically
  prop.points = as.data.frame(prop.points)
  colnames(prop.points) = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  list(prop.points = prop.points, prop.hv.contrs = prop.hv.contrs)
}
