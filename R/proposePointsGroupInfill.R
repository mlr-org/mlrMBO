proposePointsGroupInfill = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  m = control$propose.points
  n = control$multipoint.groupinfill.sample.n
  xs = generateRandomDesign(par.set = getOptProblemParSet(opt.problem), n*m)
  xs = convertDataFrameCols(xs, ints.as.num = TRUE, logicals.as.factor = TRUE)
  xs = split(xs, rep(seq_len(n), each = m))
  group.infill.crit = simpleGroupInfillCrit
  st = system.time({
    infill.results = parallelMap(group.infill.crit, points = xs, level = "mlrMBO.propose.points", more.args = list(opt.state = opt.state))
  })
  infill.crits = extractSubList(infill.results, "infill.crit", simplify = TRUE)
  best.index = which.min(infill.crits)
  prop.points = xs[[best.index]]
  list(
    prop.points = prop.points,
    propose.time = c(st[3L], rep(0, times = m-1L)),
    prop.type = rep(paste0("grouped.infill_", control$infill.crit), times = m),
    crit.vals = infill.results[[best.index]]$single.crit.vals,
    crit.components = infill.results[[best.index]]$crit.components,
    errors.model = NA_character_
  )
}


simpleGroupInfillCrit = function(points, opt.state, ...) {
  models = getOptStateModels(opt.state)$models
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  infill.crit.fun = getInfillCritFunction(control$infill.crit)
  par.set = getOptProblemParSet(opt.problem)
  models = if (inherits(models, "WrappedModel")) list(models) else models
  opt.path = getOptStateOptPath(opt.state)
  design = convertOptPathToDf(opt.path, control)
  iter = iter = getOptStateLoop(opt.state)
  crit.vals = infill.crit.fun(points, models, control, par.set, design, iter, attributes = TRUE, ...)
  infill.sum = sum(crit.vals)
  dist.penalty = mean(cluster::daisy(points, metric = "gower"))
  list(
    infill.crit = infill.sum * control$multipoint.groupinfill.dist.penalty * dist.penalty,
    single.crit.vals = crit.vals,
    crit.components = attr(crit.vals, "crit.components")
  )

}

# proposePointsQuadraticKnapsack = function(opt.state) {
#   opt.problem = getOptStateOptProblem(opt.state)
#   control = getOptProblemControl(opt.problem)
#   infill.crit.fun = getInfillCritFunction(control$infill.crit)
#   models = getOptStateModels(opt.state)$models
#   models = if (inherits(models, "WrappedModel")) list(models) else models
#   par.set = getOptProblemParSet(opt.problem)
#   design = convertOptPathToDf(opt.path, control)
#   iter = iter = getOptStateLoop(opt.state)

#   m = control$propose.points
#   n = control$multipoint.groupinfill.sample.n

#   xs = generateRandomDesign(par.set = getOptProblemParSet(opt.problem), n)

#   profits = infill.crit.fun(xs, models, control, par.set, design, iter, attributes = TRUE, ...)

#   distances = as.matrix(cluster::daisy(xs, metric = "gower"))

#   cancelation = outer(profits, profits, FUN = function(X,Y) X + Y) * exp(-distances)

#   callQKFunction(profits, cancelation, TIME???)
#   # get in return
#   # list with jobs for each CPU
#   # order in how they should be executed
#   # cpu1 = c(590, 4, 99, 188)
#   # cpu2 = c(110, 1, 160, 55)
#   # ...
# }
