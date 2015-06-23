#' @title Single-Objective result object.
#'
#' @description
#'
#' \itemize{
#'   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#'   \item{y [\code{numeric(1)}]}{Value of objective function at \code{x},
#'     either from evals during optimization or from requested final evaluations,
#'     if those were greater than 0.}
#'     \item{best.ind [\code{numeric(1)}]}{Index of \code{x} in the opt.path.}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.
#'     Includes all evaluated points and additional information.
#'     You can convert it via \code{as.data.frame}.}
#'   \item{resample.results [List of \code{\link[mlr]{ResampleResult}}]}{List of the desired \code{resample.results} if \code{resample.at} is set in \code{makeMBOControl}.}
#'   \item{final.state [\code{character}] The final termination state. Gives information why the optimization ended}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models if \code{store.model.at} is set in \code{makeMBOControl}. The default is that it contains the model generated after the last iteration.}
#'   \item{control [\code{MBOControl}] Control object used in optimization}
#' }
#' @name MBOSingleObjResult
#' @rdname MBOSingleObjResult
NULL

makeMBOResult.TuningState = function(tuningState) {
  tuningProblem = getTuningStateTuningProblem(tuningState)
  control = getTuningProblemControl(tuningProblem)
  final.points = getTuningStateFinalPoints(tuningState)
  tuningResult = getTuningStateTuningResult(tuningState)

  if (length(final.points$x)) {
    if (getTuningProblemControl(tuningProblem)$final.evals > 0) {
      ys = evalFinalPoint(tuningState, final.points)
      final.points$y = mean(ys)
    }
    makeS3Obj(
      c("MBOSingleObjResult", "MBOResult"),
      x = final.points$x,
      y = final.points$y, # strip name
      best.ind = final.points$best.ind,
      opt.path = getTuningStateOptPath(tuningState),
      resample.results = getTuningResultResampleResults(tuningResult),
      final.state = getTuningStateState(tuningState),
      models = getTuningResultStoredModels(tuningResult),
      control = control
    )
  } else {
    makeS3Obj(c("MBOMultiObjResult", "MBOResult"),
      pareto.front = final.points$pareto.front,
      pareto.set = final.points$pareto.set,
      pareto.inds = final.points$inds,
      opt.path = getTuningStateOptPath(tuningState),
      final.state = getTuningStateState(tuningState),
      models = getTuningResultStoredModels(tuningResult),
      control = control
    )
  }
}


#' @export
print.MBOResult = function(x, ...) {
  op = x$opt.path
  catf("Recommended parameters:")
  catf(paramValueToString(op$par.set, x$x))
  catf("Objective: %s = %.3f\n", op$y.names[1], x$y)
  catf("Optimization path")
  n1 = sum(op$env$dob == 0)
  n2 = length(op$env$dob) - n1
  catf("%i + %i entries in total, displaying last 10 (or less):", n1, n2)
  print(tail(as.data.frame(op), 10))
}

#' @title Multi-Objective result object.
#'
#' @description
#'
#' \itemize{
#'   \item{pareto.front [\code{matrix}]}{Pareto front of all evaluated points.}
#'   \item{pareto.set [\code{list} of \code{list}s]}{Pareto set of all evaluated points.}
#'   \item{pareto.inds [\code{numeric}]}{Indizes of the Pareto-optimal points in the opt.path}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.
#'     Includes all evaluated points and additional information.
#'     You can convert it via \code{as.data.frame}.}
#'   \item{final.state [\code{character}] The final termination state. Gives information why the optimization ended}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#'   \item{control[\code{MBOControl}] Control object used in optimization}
#' }
#' @name MBOMultiObjResult
#' @rdname MBOMultiObjResult
NULL

#' @export
print.MBOMultiObjResult = function(x, ...) {
  op = x$opt.path
  print(x$pareto.front)
  catf("Optimization path")
  n1 = sum(op$env$dob == 0)
  n2 = length(op$env$dob) - n1
  catf("%i + %i entries in total, displaying last 10 (or less):", n1, n2)
  print(tail(as.data.frame(x$opt.path), 10))
}
