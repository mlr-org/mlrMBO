

# NSGA 2
infillOptMultiCritNSGA2 = function(infill.crit, models, control, par.set, opt.path, design, iter, ...) {
  
  # build target function for vectorized nsga2 and run it
  rep.pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  m = control$number.of.targets
  control2 = control
  fun.tmp = function(x) {
    newdata = as.data.frame(x)
    colnames(newdata) = rep.pids
    asMatrixCols(lapply(1:m, function(i) {
      # we need to make sure mininimize in control is a scalar, so we can multiply it in infill crits...
      control2$minimize = control$minimize[i]
      infill.crit(points = newdata, model = models[[i]], control = control2,
        par.set = par.set, design = design, iter = iter, ...)
    }))
  }
  res = nsga2_vectorized(fun.tmp, idim = getParamNr(par.set, devectorize = TRUE), odim = control$number.of.targets,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = control$infill.opt.nsga2.popsize, generations = control$infill.opt.nsga2.generations,
    cprob = control$infill.opt.nsga2.cprob, cdist = control$infill.opt.nsga2.cdist,
    mprob = control$infill.opt.nsga2.mprob, mdist = control$infill.opt.nsga2.mdist, ...)
  points = as.data.frame(res$par)
  colnames(points) = rep.pids
  
  # now we have nsga2.popsize candidate points and we have to select propose.points
  # do this greedy - select the point with max. hv.contr, add it and select
  # the best point wrt to the new front
  candidate.points = res$par
  candidate.vals = res$value
  prop.points = matrix(nrow = 0, ncol = ncol(candidate.points))
  prop.vals = matrix(nrow = 0, ncol = ncol(candidate.vals))
  colnames(prop.vals) = control$y.name
  ys = design[, control$y.name]
  
  ref.point = getMultiCritRefPoint(design[, control$y.name], control)
  for (i in 1:control$propose.points) {
    hv.contrs = getHypervolumeContributions(xs = candidate.vals,
      ys = rbind(ys, prop.vals), ref.point = ref.point, minimize = control$minimize)
    best.ind = which.max(hv.contrs)
    # add best to prop.points/vals and remove from candidate.point/vals
    prop.points = rbind(prop.points, candidate.points[best.ind, ])
    candidate.points = candidate.points[-best.ind, ]
    prop.vals = rbind(prop.vals, candidate.vals[best.ind, ])
    candidate.vals = candidate.vals[-best.ind, ]
  }
  
  # FIXME: cleanup - i'm reall unsure how to set the names of prop.points technically
  prop.points = as.data.frame(prop.points)
  colnames(prop.points) = names(design[, which(colnames(design) %nin% control$y.name)])
  return(prop.points)
}
