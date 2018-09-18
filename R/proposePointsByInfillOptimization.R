# directly call an optimizer on an infill crit and output 1 or more proposed points
#
# input: models               : EITHER a single model or a list of models, depending on the method
#
# output:
#   prop.points [data.frame]  : the proposed points, 1 per row, with n rows
#   crit.vals [matrix(n, k)]  : crit vals for proposed points. rows = points
#                               for some methods, we have a cv for each objective.
#                               in this case k > 1 and typically k = n.objectives
#   propose.time [numeric(n)] : time needed to propose points(s)
#   errors.models [character] : model errors, resulting in randomly proposed points.
#                               length is one string PER PROPOSED POINT, not per element of <models>
#                               NA if the model was Ok, or the (first) error message if some model crashed
proposePointsByInfillOptimization = function(opt.state, par.set = NULL, control = NULL, opt.path = NULL, models = NULL, designs = NULL, ...) {
  opt.problem = getOptStateOptProblem(opt.state)
  models = models %??% getOptStateModels(opt.state)$models
  if (inherits(models, "WrappedModel")) models = list(models)
  par.set = par.set %??% getOptProblemParSet(opt.problem)
  designs = designs %??% getOptStateDesigns(opt.state)
  if (inherits(designs, "data.frame")) designs = list(designs)
  control = control %??% getOptProblemControl(opt.problem)
  opt.path = opt.path %??% getOptStateOptPath(opt.state)
  iter = getOptStateLoop(opt.state)
  infill.crit.id = getMBOInfillCritId(control$infill.crit)
  progress = getOptStateProgress(opt.state)

  n = control$propose.points
  prop.type = rep(paste0("infill_", infill.crit.id), n)

  # ensure we have a list
  ch = checkFailedModels(models, par.set, n, control = control)
  if (!ch$ok) return(ch$prop)

  infill.crit.fun = control$infill.crit$fun
  infill.opt.fun = getInfillOptFunction(control$infill.opt)
  # store time to propose single point
  secs = measureTime({
    prop.points = infill.opt.fun(infill.crit.fun, models = models,
      control = control, par.set = par.set, opt.path = opt.path,
      designs = designs, iter = iter, progress = progress, ...)
  })
  prop.points.converted = convertDataFrameCols(prop.points, ints.as.num = TRUE,
    logicals.as.factor = TRUE)
  crit.vals = infill.crit.fun(prop.points.converted, models, control, par.set,
    designs, iter, progress = progress, attributes = TRUE, ...)
  crit.components = attr(crit.vals, "crit.components")
  crit.vals = matrix(crit.vals, ncol = 1L)

  makeProposal(
    control = control,
    prop.points = prop.points,
    propose.time = secs,
    prop.type = prop.type,
    crit.vals = crit.vals,
    crit.components = crit.components
  )
}
