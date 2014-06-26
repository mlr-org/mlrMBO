generateMBOMultiFidDesign = function(par.set, control){
  budget = control$init.design.points
  par.set.lower = parSetWithout(par.set, control$multifid.param)
  k = length(control$multifid.lvls)
  b = ceiling(budget / k)
  ns = viapply(chunk(seq_len(budget), n.chunks=k), length)
  design = generateDesign(max(ns), par.set.lower, fun = control$init.design.fun,
                          fun.args = control$init.design.args, trafo = FALSE)
  expandDesign(design = design, control = control, ns = ns)
}

# convert opt path to a data.frame, maybe drop technical columns so we only have features and y
convertOptPathToDesign = function(opt.path, drop=TRUE) {
  d = as.data.frame(opt.path, discretes.as.factor = TRUE)
  drop.names = setdiff(colnames(d), c(opt.path$y.names, names(opt.path$par.set$pars)))
  if (drop)
    dropNamed(d, drop.names)
  else
    d
}

proposePoints.MultiFid = function(model, par.set, control, opt.path, model.cor, model.cost, model.sd){
  lapply(control$multifid.lvls, function(v) {
    this.par.set = par.set
    this.par.set$pars[[control$multifid.param]]$lower = v
    this.par.set$pars[[control$multifid.param]]$upper = v
    proposePoints(model = model, par.set = this.par.set, control = control, opt.path = opt.path, model.cor = model.cor, model.cost = model.cost, model.sd = model.sd)
  })
}

convertOptPathToTask = function(opt.path, control, drop=TRUE) {
  d = convertOptPathToDesign(opt.path)
  makeRegrTask(id="surrogate", data=d, target="y")
}

parSetWithout = function(par.set, without) {
  res = par.set
  res$pars = Filter(function(p) p$id %nin% control$multifid.param, res$pars)
  res
}

expandDesign = function(design, control, ns = NULL) {
  if(is.null(ns)){
    ns = rep(ncol(design), times = length(control$multifid.lvls))
  }
  designs = lapply(seq_along(ns), function(i) {
    des = design[seq_len(ns[i]),, drop=FALSE]
    des[[control$multifid.param]] = control$multifid.lvls[i]
    des
  })
  do.call(rbind.data.frame, designs)
}


# return only crit vector
infillCritMultiFid = function(points, model, control, par.set, design, model.cor, model.sd, model.cost, ...) {
  infillCritMultiFid2(points, model, control, par.set, design, model.cor, model.sd, model.cost, ...)$crit
}

# return all crap so we can plot it later
infillCritMultiFid2 = function(points, model, control, par.set, design, model.cor, model.sd, model.cost, ...) {
  # note: mbo returns the negated EI (and SE), so have to later minimize the huang crit.
  # which is done by default by our optimizer anyway
  ei.last = infillCritEI(points=points, model=model, control=control, par.set=par.set, design=design)
  alpha1 = replaceByList(points[[control$multifid.param]], model.cor)
  se = -infillCritStandardError(points=points, model=model, control=control, par.set=par.set, design=design)
  # FIXME: do we really have to adapt this? alpha2 should be 0 when?
  model.sd.vec = replaceByList(points[[control$multifid.param]], model.sd)
  alpha2 = 1 - (sqrt(2) * model.sd.vec / sqrt(se^2 + model.sd.vec^2))
  alpha3 = replaceByList(points[[control$multifid.param]], model.cost)
  crit = ei.last * alpha1 * alpha2 * alpha3
  list(crit=crit, ei=ei.last, se=se, alpha1=alpha1, alpha2=alpha2, alpha3=alpha3)
}

replaceByList = function(x, rep.list){
  x = as.character(x)
  res = rep(rep.list[[1]], times = length(x)) #res gets the right type
  for(key in names(rep.list)) {
    res[x == key] = rep.list[[key]]
  }
  res
}
