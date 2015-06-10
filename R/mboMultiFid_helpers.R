# generates initial design for multifid
generateMBOMultiFidDesign = function(par.set, control) {
  design = generateDesign(control$init.design.points, par.set, fun = control$init.design.fun, fun.args = control$init.design.args, trafo = FALSE)
}

# convert OP to a df, of a specified structure with col .multifid.lvl, so we can model on it
convertMFOptPathToDesign = function(opt.path, ...) {
  as.data.frame(opt.path, include.rest = FALSE, discretes.as.factor = TRUE, ...)
}

# convert OP to a mlr task, of a specified structure with col .multifid.lvl, so we can model on it
convertMFOptPathToTask = function(opt.path, ...) {
  d = convertMFOptPathToDesign(opt.path, ...)
  makeRegrTask(id = "multifid.task", data = d, target = "y")
}

convertMFOptPathToTimeTask = function(opt.path, ...) {
  time.data = convertMFOptPathToDesign(opt.path, include.y = FALSE, ...)
  time.data$exec.time = getOptPathExecTimes(opt.path)
  makeRegrTask(id = "time.task", data = time.data, target = "exec.time")
}

# propose Points for each multifid level. return a list
proposePointsMultiFid = function(model, par.set, control, opt.path, iter) {
  # lvl.cors, lvl.sds
  lvl.sds = vnapply(seq_along(control$multifid.lvls), calcModelSD, model = model, par.set = par.set, control = controll, opt.path = opt.ptah)
  corgrid = generateDesign(n = control$multifid.cor.grid.points, par.set = par.set)
  lvl.cors = vnapply(seq_along(control$multifid.lvls), calcModelCor, grid = corgrid, nlvls = length(control$multifid.lvls))
  contorl.nomf = control
  control.nomf$multifid = FALSE
  par.set.nomf = dropNamed(par.set, ".multifid.lvl")
  lapply(seq_along(control$multifid.lvls), function(lvl) {
    res = proposePointsByInfillOptimization(model, par.set.nomf, control, opt.path, iter = iter, lvl.cors = lvl.cors, lvl.sds = lvl.sds, lvl = lvl)
    res$prop.points$.multifid.lvl = lvl
    res
  })
}

# FIXME: Should be implemented smarter with the already existent filterProposedPoints function
filterProposedPointsMultiFid = function(prop, opt.path, par.set, control, lvl) {
  # prepare stuff
  n = nrow(prop$prop.points)
  design = getOptPathX(opt.path)
  design = design[design$.multifid.lvl == lvl,]
  design = dropNamed(design, ".multifid.lvl")
  calcMaxMetric = function(x, y) max(abs(x - y))
  to.delete = rep(FALSE,  n)

  # look at min distance from i-point to current set (design + accepted)
  for (i in 1:n) {
    pp = prop$prop.points[i, ]
    min.dist = min(apply(design, 1, calcMaxMetric, y = pp))
    # if too close, mark i-point, otherwise add it to set
    if (min.dist < control$filter.proposed.points.tol)
      to.delete[i] = TRUE
    else
      design = rbind(design, pp)
  }

  # for now replace removed design points with random points,
  #  we leave all other data in prop like it is, we have flag filter.replace
  n.replace = sum(to.delete)
  prop$filter.replace = to.delete

  if (n.replace > 0) {
    counter = 0
    repeat {
      prop$prop.points[to.delete, ] = generateRandomDesign(n.replace, par.set)
      min.dist = min(apply(design, 1, calcMaxMetric, y = pp))
      counter = counter + 1
      if(min.dist > control$filter.proposed.points.tol || counter > 20L)
        break
    }
    # FIXME: we might want to do something smarter here. how about augmenting the current design?
    
  }

  return(prop)
}

# return only crit vector
infillCritMultiFid = function(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, lvl) {
  infillCritMultiFid2(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, lvl)$crit
}

# return all crap so we can plot it later
infillCritMultiFid2 = function(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, lvl) {
  nlvls = length(control$multifid.lvls)
  points.current = cbind(points, .multifid.lvl = lvl)
  points.last = cbind(points, .multifid.lvl = nlvls) #points on most expensive level
  design.last = design[design$.multifid.lvl == nlvls, , drop = FALSE]
  # note: mbo returns the negated EI (and SE), so have to later minimize the huang crit.
  # which is done by default by our optimizer anyway
  infill.crit.fun = getInfillCritFunction(control)
  ei.last = infill.crit.fun(points.last, model, control, par.set, design.last, iter)
  alpha1 = lvl.cors[lvl]

  # ALPHA 2
  se = -infillCritStandardError(points.current, model, control, par.set, NULL, iter)
  if (any(lvl.sds < 0.01)) { # FIXME: IF lvl.sd near 0 it will make alpha2 useless
    lvl.sds = lvl.sds + 0.01
  }
  sigmas = lvl.sds[lvl]
  alpha2 = 1 - (sigmas / sqrt(se^2 + sigmas^2))

  # ALPHA 3
  alpha3 = getLast(control$multifid.costs) / control$multifid.costs[lvl]
  crit = ei.last * alpha1 * alpha2 * alpha3
  list(crit = crit, ei = ei.last, se = se, alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, sd = sigmas)
}

calcModelSD = function(lvl, model, par.set, control, opt.path) {
    newdata = convertOptPathToDf(par.set, opt.path, control)
    newdata = newdata[newdata$.multifid.lvl == lvl, ]
    sqrt(estimateResidualVariance(model, data = newdata, target = "y"))
}

# calculate GLOBAL correlation between model w/ lvl and last-lvl. currently rank correlation.
calcModelCor = function(lvl, model, grid, nlvls) {
  new.grid = rbind.data.frame(
    cbind.data.frame(grid, .multifid.lvl = lvl),
    cbind.data.frame(grid, .multifid.lvl = nlvls)
  )
  p = predict(model, newdata = new.grid)$data$response
  p1 = p[new.grid$.multifid.lvl == lvl]
  p2 = p[new.grid$.multifid.lvl == nlvls]
  
  # check whether vectors are constant, cor = NA then
  if (diff(range(p1)) < sqrt(.Machine$double.eps) || diff(range(p2)) < sqrt(.Machine$double.eps))
    0
  else
    max(cor(p1, p2, method = "spearman"), 0)
}
