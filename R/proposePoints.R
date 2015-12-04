# Propose infill points - simple dispatcher to real methods
#
# input:
#   tasks [list]              : list of gnerated tasks
#   models [list]             : list of models, fitted to tasks
#
# output:
#   prop.points [data.frame]  : the proposed points, 1 per row, with n rows
#   crit.vals [matrix(n, k)]  : crit vals for proposed points. rows = points
#                               for some methods, we have a cv for each objective.
#                               in this case k > 1 and typically k = number.of.targets
#   errors.models [character] : model errors, resulting in randomly proposed points.
#                               length is one string PER PROPOSED POINT, not per element of <models>
#                               NA if the model was Ok, or the (first) error message if some model crashed
proposePoints.OptState = function(opt.state, control = NULL){

  opt.problem = getOptStateOptProblem(opt.state)
  control = coalesce(control, getOptProblemControl(opt.problem))

  m = control$number.of.targets
  res = NULL
  if (m == 1L && control$infill.crit != "random") {
    if (control$multifid) {
      res = proposePointsMultiFid(opt.state)
    } else if (is.null(control$multipoint.method)) {
      res = proposePointsByInfillOptimization(opt.state)
    } else {
      if (control$multipoint.method == "lcb")
        res = proposePointsParallelLCB(opt.state)
      else if (control$multipoint.method == "cl")
        res = proposePointsConstantLiar(opt.state)
      else if (control$multipoint.method == "multicrit") {
        res = proposePointsMOIMBO(opt.state)
      }
    }
  } else if (control$infill.crit != "random"){
    if (control$multicrit.method == "parego") {
      res = proposePointsParEGO(opt.state)
    } else if (control$multicrit.method == "mspot") {
      res = proposePointsMSPOT(opt.state)
    } else if (control$multicrit.method == "dib") {
      res = proposePointsDIB(opt.state)
    }
  }
  
  if (control$infill.crit == "random" || control$interleave.random.points > 0L) {
    add = proposePointsRandom(opt.state, control)
    if (!is.null(res))
      res = joinProposedPoints(list(res, add))
    else
      res = add
  }

  if (!is.matrix(res$crit.vals))
    res$crit.vals = matrix(res$crit.vals, ncol = 1L)
  
  if (control$filter.proposed.points) {
    res = filterProposedPoints(res, opt.state)
  }

  if (control$schedule.method != "none") {
    time.model = getOptStateTimeModel(opt.state)
    time.prediction = predict(time.model, newdata = res$prop.points)
    res$predicted.time = getPredictionResponse(time.prediction)
    res$predicted.time.se = getPredictionSE(time.prediction)
  }
  res
}