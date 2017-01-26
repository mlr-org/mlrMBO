# Propose infill points - simple dispatcher to real methods
#
# input:
#   opt.state
#
# output:
#   prop.points [data.frame]  : the proposed points, 1 per row, with n rows
#   crit.vals [matrix(n, k)]  : crit vals for proposed points. rows = points
#                               for some methods, we have a cv for each objective.
#                               in this case k > 1 and typically k = n.objectives
#   errors.models [character] : model errors, resulting in randomly proposed points.
#                               length is one string PER PROPOSED POINT, not per element of <models>
#                               NA if the model was Ok, or the (first) error message if some model crashed
proposePoints.OptState = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  m = control$n.objectives
  res = NULL
  if (m == 1L) {
    # if (control$multifid) {
    #   res = proposePointsMultiFid(opt.state)
    if (is.null(control$multipoint.method)) {
      res = proposePointsByInfillOptimization(opt.state)
    } else {
      if (control$multipoint.method == "cb")
        res = proposePointsParallelCB(opt.state)
      else if (control$multipoint.method == "cl")
        res = proposePointsConstantLiar(opt.state)
      else if (control$multipoint.method == "moimbo") {
        res = proposePointsMOIMBO(opt.state)
      }
    }
  } else {
    if (control$multiobj.method == "parego") {
      res = proposePointsParEGO(opt.state)
    } else if (control$multiobj.method == "mspot") {
      res = proposePointsMSPOT(opt.state)
    } else if (control$multiobj.method == "dib") {
      res = proposePointsDIB(opt.state)
    }
  }

  if (control$interleave.random.points > 0L) {
    add = proposePointsRandom(opt.state)
    res = joinProposedPoints(list(res, add))
  }

  if (!is.matrix(res$crit.vals))
    res$crit.vals = matrix(res$crit.vals, ncol = 1L)

  if (control$filter.proposed.points) {
    res = filterProposedPoints(res, opt.state)
  }
  res
}
