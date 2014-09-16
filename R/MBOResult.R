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
#'     FIXME: Finish doc here
#'   \item{resample.vals ???}{???}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#' }
#' @name MBOSingleObjResult
#' @rdname MBOSingleObjResult
NULL

makeMBOSingleObjResult = function(final.index, opt.path, resample.results, convergence, models) {
  best = getOptPathEl(opt.path, final.index)
  x = best$x
  y = best$y

  makeS3Obj(c("MBOSingleObjResult", "MBOResult"),
    x = best$x,
    y = as.numeric(best$y), # strip name
    best.ind = final.index,
    opt.path = opt.path,
    resample.results = resample.results,
    convergence = convergence,
    models = models
  )
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
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#' }
#' @name MBOMultiObjResult
#' @rdname MBOMultiObjResult
NULL

makeMBOMultiCritResult = function(opt.path, convergence, models) {
  # get indices of pareto front from path, then add rest
  inds = getOptPathParetoFront(opt.path, index = TRUE)
  res = makeS3Obj(c("MBOMultiObjResult", "MBOResult"),
    pareto.front = getOptPathY(opt.path)[inds, , drop = FALSE],
    pareto.set = lapply(inds, function(i) getOptPathEl(opt.path, i)$x),
    pareto.inds = inds,
    opt.path = opt.path,
    convergence = convergence,
    models = models
  )
}

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



