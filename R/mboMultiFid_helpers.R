# generates initial design for multifid
# currently we do: equal nr of points for each level, generate the same X points for each level
generateMBOMultiFidDesign = function(par.set, control) {
  n = control$init.design.points
  k = length(control$multifid.lvls)
  npoints.per.lvl = viapply(chunk(1:n, n.chunks = k), length)
  # generate the points for the lowest level
  design = generateDesign(max(npoints.per.lvl), ps, fun = control$init.design.fun,
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
  all.inds = 1:n
  designs = lapply(1:k, function(i) {
    nppl = npoints.per.lvl[i]
    # do we need to drop some rows of design? lets also keep the order of all.inds
    inds = if (nppl < n)
      set.diff(all.inds, sample(all.inds, n - nppl))
    else
      all.inds
    cbind(design[inds, , drop = FALSE], .multifid.lvl = i)
  })
  do.call(rbind.data.frame, designs)
}

# convert opt path to a data.frame, maybe drop technical columns so we only have features and y
convertOptPathToDesign = function(opt.path, drop = TRUE) {
  d = as.data.frame(opt.path, discretes.as.factor = TRUE)
  drop.names = setdiff(colnames(d), c(opt.path$y.names, names(opt.path$par.set$pars)))
  if (drop)
    dropNamed(d, drop.names)
  else
    d
}

# propose Points for each multifid level. return a list
proposePointsMultiFid = function(model, par.set, control, opt.path, model.cor, model.cost, model.sd) {
  lapply(control$multifid.lvls, function(v) {
    this.par.set = par.set
    this.par.set$pars[[control$multifid.param]]$lower = v
    this.par.set$pars[[control$multifid.param]]$upper = v
    proposePointsByInfillOptimization(models = model, par.set = this.par.set, control = control, opt.path = opt.path,
      model.cor = model.cor, model.cost = model.cost, model.sd = model.sd)
  })
}

convertOptPathToTask = function(opt.path, control = NULL, drop = TRUE, blocking = TRUE) {
  d = convertOptPathToDesign(opt.path, drop = drop)
  if (!is.null(control) && !is.null(control$multifid.param) && blocking) {
    id.vars = setdiff(colnames(d), c("y", control$multifid.param))
    d.blocking = unique(d[, id.vars, drop = FALSE])
    d.blocking$blocking = factor(seq_row(d.blocking))
    d.blocking = merge(d, d.blocking)
    makeRegrTask(id = "surrogate", data = d, target = "y", blocking = d.blocking$blocking)
  } else {
    makeRegrTask(id = "surrogate", data = d, target = "y")
  }
}


# return only crit vector
infillCritMultiFid = function(points, model, control, par.set, design, iter, model.cor, model.sd, model.cost, ...) {
  infillCritMultiFid2(points, model, control, par.set, design, iter, model.cor, model.sd, model.cost, ...)$crit
}

# return all crap so we can plot it later
infillCritMultiFid2 = function(points, model, control, par.set, design, iter, model.cor, model.sd, model.cost, ...) {
  lastPoints = function(x) {
    x[, control$multifid.param] = tail(control$multifid.lvls, 1)
    x
  }
  # note: mbo returns the negated EI (and SE), so have to later minimize the huang crit.
  # which is done by default by our optimizer anyway
  ei.last = infillCritEI(lastPoints(points), model, control, par.set, lastPoints(design), iter)
  alpha1 = replaceByList(points[, control$multifid.param], model.cor)
  se = -infillCritStandardError(points, model, control, par.set, design, iter)
  # FIXME: do we really have to adapt this? alpha2 should be 0 when?
  model.sd.vec = replaceByList(points[[control$multifid.param]], model.sd) #FIXME: Make 100% sure
  alpha2 = 1 - (model.sd.vec / sqrt(se^2 + model.sd.vec^2))
  alpha3 = replaceByList(points[[control$multifid.param]], model.cost)
  crit = ei.last * alpha1 * alpha2 * alpha3
  list(crit = crit, ei = ei.last, se = se, alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, sd = model.sd.vec)
}

# replaces the values a,b,c in x by the values r,s,t from the rep.list = list(a=r,b=s,c=t) with replications in x allowed
replaceByList = function(x, rep.list) {
  x = as.character(x)
  res = rep(rep.list[[1]], times = length(x)) #res gets the right type
  for (key in names(rep.list)) {
    res[x == key] = rep.list[[key]]
  }
  res
}
