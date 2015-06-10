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
