# generates initial design for multifid
generateMBOMultiFidDesign = function(par.set, control) {
  design = generateDesign(control$init.design.points, par.set, fun = control$init.design.fun, fun.args = control$init.design.args, trafo = FALSE)
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
proposePointsMultiFid = function(model, par.set, control, opt.path, lvl.cors, time.model, lvl.sds) {
  lapply(seq_along(control$multifid.lvls), function(lvl) {
    res = proposePointsByInfillOptimization(model, par.set, control, opt.path,
      lvl.cors = lvl.cors, time.model = time.model, lvl.sds = lvl.sds, lvl = lvl)
    res$prop.points$.multifid.lvl = lvl
    res
  })
}

# return only crit vector
infillCritMultiFid = function(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, time.model, lvl) {
  infillCritMultiFid2(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, time.model, lvl)$crit
}

# return all crap so we can plot it later
infillCritMultiFid2 = function(points, model, control, par.set, design, iter, lvl.cors, lvl.sds, time.model, lvl) {
  nlvls = length(control$multifid.lvls)
  points.current = cbind(points, .multifid.lvl = lvl)
  points.last = cbind(points, .multifid.lvl = nlvls) #points on most expensive level
  design.last = design[design$.multifid.lvl == nlvls, , drop = FALSE]
  # note: mbo returns the negated EI (and SE), so have to later minimize the huang crit.
  # which is done by default by our optimizer anyway
  ei.last = infillCritAEI(points.last, model, control, par.set, design.last, iter)
  cost.current = infillCritMeanResponse(points.current, time.model, control)
  cost.last = infillCritMeanResponse(points.last, time.model, control)
  if(all(ei.last == 0)) {
    # warning("All EI = 0!")
  }
  alpha1 = lvl.cors[lvl]
  se = -infillCritStandardError(points.current, model, control, par.set, NULL, iter)
  taus = lvl.sds[lvl]
  alpha2 = 1 - (taus / sqrt(se^2 + taus^2))
  if (!is.null(control$multifid.costs)) {
    alpha3 = getLast(control$multifid.costs) / control$multifid.costs[lvl]
  } else {
    alpha3 = cost.last / cost.current 
  }
  crit = ei.last * alpha1 * alpha2 * alpha3
  list(crit = crit, ei = ei.last, se = se, alpha1 = alpha1, alpha2 = alpha2, alpha3 = alpha3, sd = taus)
}

# time cost model
multiFidTimeCosts = function(cur, last, opt.path, grid) {
  time.data = convertMFOptPathToDesign(opt.path, include.y = FALSE)
  time.data$exec.time = getOptPathExecTimes(opt.path)
  time.learner = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)
  time.task = makeRegrTask(id = "multifid.task", data = time.data, target = "exec.time")
  time.model = train(time.learner, task = time.task)
  #grid = generateDesign(n = control$multifid.cor.grid.points, par.set = par.set)
  grid = do.call(rbind, lapply(c(cur,last), function(x) cbind(grid, .multifid.lvl = x)))
  p = predict(time.model, newdata = grid)$data$response
  p = split(p, grid$.multifid.lvl)
  times = sapply(p, mean)
  times[2]/times[1]
}
