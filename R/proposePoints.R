#' @title Propose candidates for the objective function
#' @description Propose points for the objective function that should be evaluated according to the infill criterion and the recent evaluations.
#'
#' @param opt.state [\code{\link{OptState}}]
#' @export
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
proposePoints = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  if (control$n.objectives == 1L) {
    if (is.null(control$multipoint.method)) {
      res = proposePointsByInfillOptimization(opt.state)
    } else {
      res = switch(control$multipoint.method,
        "cb" = proposePointsParallelCB(opt.state),
        "cl" = proposePointsConstantLiar(opt.state),
        "moimbo" = proposePointsMOIMBO(opt.state)
      )
    }
  } else {
    res = switch(control$multiobj.method,
      "parego" = proposePointsParEGO(opt.state),
      "mspot"  = proposePointsMSPOT(opt.state),
      "dib"    = proposePointsDIB(opt.state)
    )
  }

  if (control$interleave.random.points > 0L)
    res = joinProposedPoints(list(res, proposePointsRandom(opt.state)))

  if (!is.matrix(res$crit.vals))
    res$crit.vals = matrix(res$crit.vals, ncol = 1L)

  if (control$filter.proposed.points)
    res = filterProposedPoints(res, opt.state)

  res
}
