# generates initial design for multifid
# currently we do: equal nr of points for each level, generate the same X points for each level
generateMBOMultiFidDesign = function(par.set, control) {
  n = control$init.design.points
  k = length(control$multifid.lvls)
  npoints.per.lvl = viapply(chunk(seq_len(n), n.chunks = k), length)
  # generate the points for the lowest level
  design = generateDesign(max(npoints.per.lvl), par.set, fun = control$init.design.fun,
    fun.args = control$init.design.args, trafo = FALSE)
  expandDesign(design = design, control = control, npoints.per.lvl = npoints.per.lvl)
}

# make a design (data.frame) with the exact same points for each multifid level.
# if the requested number of points is a bit lower than design, we randomly drop some rows
expandDesign = function(design, control, npoints.per.lvl = NULL) {
  n = nrow(design)
  k = length(control$multifid.lvls)
  # default is to replicate the design for all levels
  if (is.null(npoints.per.lvl))
    npoints.per.lvl = rep(n, times = k)
  all.inds = seq_len(n)
  designs = lapply(seq_len(k), function(i) {
    nppl = npoints.per.lvl[i]
    # do we need to drop some rows of design? lets also keep the order of all.inds
    inds = if (nppl < n)
      setdiff(all.inds, sample(all.inds, n - nppl))
    else
      all.inds
    cbind(design[inds, , drop = FALSE], .multifid.lvl = i)
  })
  do.call(rbind.data.frame, designs)
}

# convert OP to a df, of a specified structure with col .multifid.lvl, so we can model on it
convertMFOptPathToDesign = function(opt.path) {
  as.data.frame(opt.path, include.rest = FALSE, discretes.as.factor = TRUE)
}

# convert OP to a mlr task, of a specified structure with col .multifid.lvl, so we can model on it
convertMFOptPathToTask = function(opt.path) {
  d = convertMFOptPathToDesign(opt.path)
  makeRegrTask(id = "multifid.task", data = d, target = "y")
}

# propose Points for each multifid level. return a list
proposePointsMultiFid = function(model, par.set, control, opt.path, lvl.cors, lvl.costs, lvl.sds) {
  lapply(seq_along(control$multifid.lvls), function(lvl) {
    proposePointsByInfillOptimization(model, par.set, control, opt.path,
      lvl.cors = lvl.cors, lvl.costs = lvl.costs, lvl.sds = lvl.sds, lvl = lvl)
  })
}

# return only crit vector
infillCritMultiFid = function(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, lvl.costs, lvl) {
  infillCritMultiFid2(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, lvl.costs, lvl)$crit
}

# return all crap so we can plot it later
infillCritMultiFid2 = function(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, lvl.costs, lvl) {
  nlvls = length(control$multifid.lvls)
  points1 = cbind(points, .multifid.lvl = lvl)
  points2 = cbind(points, .multifid.lvl = nlvls) #points on most expensive level
  design2 = design[design$.multifid.lvl == nlvls, , drop = FALSE]
  # note: mbo returns the negated EI (and SE), so have to later minimize the huang crit.
  # which is done by default by our optimizer anyway
  ei.last = infillCritEI(points2, model, control, par.set, design2, iter)
  alpha1 = lvl.cors[lvl]
  se = -infillCritStandardError(points1, model, control, par.set, NULL, iter)
  taus = lvl.sds[lvl]
  alpha2 = 1 - (taus / sqrt(se^2 + taus^2))
  alpha3 = lvl.costs[lvl]
  crit = ei.last * alpha1 * alpha2 * alpha3
  list(crit = crit, ei = ei.last, se = se, alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, sd = taus)
}

