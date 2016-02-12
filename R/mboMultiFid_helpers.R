# propose Points for each multifid level. return a list
proposePointsMultiFid = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  model = getOptStateModels(opt.state)$models[[1L]]
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  iter = getOptStateLoop(opt.state)

  #FIXME: We wouldn't need that, if we could pass the opt.state to the infillCrit* functions
  if (is.null(control$multifid.costs)) {
    time.model = getOptStateTimeModel(opt.state)
  } else {
    time.model = NULL
  }

  par.set.nomf = dropParams(par.set, ".multifid.lvl")

  design = convertOptPathToDf(opt.path = opt.path, control = control)
  lvl.sds = vnapply(seq_along(control$multifid.lvls), calcModelSD, model = model, control = control, design = design)
  corgrid = generateDesign(n = control$multifid.cor.grid.points, par.set = par.set.nomf)
  lvl.cors = vnapply(seq_along(control$multifid.lvls), calcModelCor, model = model, grid = corgrid, nlvls = length(control$multifid.lvls))

  prop = lapply(seq_along(control$multifid.lvls), function(lvl) {
    res = proposePointsByInfillOptimization(opt.state, par.set = par.set.nomf, lvl.cors = lvl.cors, lvl.sds = lvl.sds, lvl = lvl, time.model = time.model)
    res$prop.points$.multifid.lvl = lvl
    res
  })

  infill.vals = extractSubList(prop, "crit.vals")

  if(iter %% control$multifid.force.last.level.steps == 0 || iter == control$iters)
    min.index = length(control$multifid.lvls)
  else
    min.index = getMinIndex(infill.vals)

  prop[[min.index]]
}

#this function allows for points including the .multifid.lvl columns
infillCritMultiFid.external = function(points, models, control, par.set, design, iter, lvl.cors, lvl.sds, lvl, time.model) {
  model = models[[1L]]
  if (".multifid.lvl" %nin% getParamIds(par.set)) {
    par.set = c(par.set, makeParamSet(
      makeIntegerParam(".multifid.lvl", lower = 1L, upper = length(control$multifid.lvls))))
  }
  lvl.sds = vnapply(seq_along(control$multifid.lvls), calcModelSD, model = model, control = control, design = design)

  corgrid = generateDesign(n = control$multifid.cor.grid.points, par.set = dropParams(par.set, ".multifid.lvl"))
  lvl.cors = vnapply(seq_along(control$multifid.lvls), calcModelCor, model = model, grid = corgrid, nlvls = length(control$multifid.lvls))
  pointssplit = split(points, points$.multifid.lvl)
  lvls.inds = sort(unique(points$.multifid.lvl))
  pointsres = lapply(seq_along(lvls.inds), function(i) {
    points.lvl = dropNamed(pointssplit[[i]], ".multifid.lvl")
    infillCritMultiFid2(points = points.lvl, models = models, control = control, par.set = par.set, design = design, iter = iter, lvl.cors = lvl.cors, lvl.sds = lvl.sds, lvl = lvls.inds[i])$crit
  })
  unsplit(pointsres, points$.multifid.lvl)
}

# return only crit vector
infillCritMultiFid = function(points, models, control, par.set, design, iter, lvl.cors, lvl.sds, lvl, time.model) {
  infillCritMultiFid2(points, models, control, par.set, design, iter, lvl.cors, lvl.sds, lvl, time.model)$crit
}

# return all crap so we can plot it later
infillCritMultiFid2 = function(points, models, control, par.set, design, iter, lvl.cors, lvl.sds, lvl, time.model) {
  nlvls = length(control$multifid.lvls)
  points.current = cbind(points, .multifid.lvl = lvl)
  points.last = cbind(points, .multifid.lvl = nlvls) #points on most expensive level
  design.last = design[design$.multifid.lvl == nlvls, , drop = FALSE]
  # note: mbo returns the negated EI (and SE), so have to later minimize the huang crit.
  # which is done by default by our optimizer anyway
  infill.crit.fun = getInfillCritFunction(control$infill.crit)
  ei.last = infill.crit.fun(points.last, models, control, par.set, design.last, iter)
  alpha1 = lvl.cors[lvl]

  # ALPHA 2
  se = -infillCritStandardError(points.current, models, control, par.set, NULL, iter)
  if (any(lvl.sds < 0.001)) { # FIXME: IF lvl.sd near 0 it will make alpha2 useless
    lvl.sds = lvl.sds + 0.001
  }
  sigmas = lvl.sds[lvl]
  alpha2 = 1 - (sigmas / sqrt(se^2 + sigmas^2))

  # ALPHA 3
  if (!is.null(control$multifid.costs)) {
    alpha3 = getLast(control$multifid.costs) / control$multifid.costs[lvl]
  } else {
    alpha3 = getPredictionResponse(predict(time.model, newdata = points.last)) / getPredictionResponse(predict(time.model, newdata = points.current))
  }
  crit = ei.last * alpha1 * alpha2 * alpha3
  list(crit = crit, ei = ei.last, se = se, alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, sd = sigmas)
}

calcModelSD = function(lvl, model, control, design) {
    newdata = design[design$.multifid.lvl == lvl, ]
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
