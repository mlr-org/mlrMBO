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
#'     Includes all evaluated points and additional information as documented in \link{mbo_OptPath}.
#'     You can convert it via \code{as.data.frame}.}
#'   \item{resample.results [List of \code{\link[mlr]{ResampleResult}}]}{List of the desired \code{resample.results} if \code{resample.at} is set in \code{makeMBOControl}.}
#'   \item{final.state [\code{character}] The final termination state. Gives information why the optimization ended. Possible values are
#'      \describe{
#'        \item{term.iter}{Maximal number of iterations reached.}
#'        \item{term.time}{Maximal running time exceeded.}
#'        \item{term.exectime}{Maximal execution time of function evaluations reached.}
#'        \item{term.yval}{Target function value reached.}
#'        \item{term.fevals}{maximal number of function evaluations reached.}
#'        \item{term.custom}{Terminated due to custom, user-defined termination condition.}
#'     }
#'   }
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models if \code{store.model.at} is set in \code{makeMBOControl}. The default is that it contains the model generated after the last iteration.}
#'   \item{control [\code{MBOControl}] Control object used in optimization}
#' }
#' @name MBOSingleObjResult
#' @rdname MBOSingleObjResult
NULL

makeMBOResult.OptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  final.points = getOptStateFinalPoints(opt.state)
  opt.result = getOptStateOptResult(opt.state)

  #FIXME: There has to be a better way to notice if it's multiobjective
  #also it's possible that with getOptStateFinalPoints(... unify = TRUE) we get final.points$x. But I don't know where we do that.
  x.df = final.points$x
  if (!is.null(x.df) && nrow(x.df) == 1) {
    if (getOptProblemControl(opt.problem)$final.evals > 0) {
      ys = evalFinalPoint(opt.state, x.df)
      final.points$y = mean(ys)
    }
    makeS3Obj(
      c("MBOSingleObjResult", "MBOResult"),
      x = dfRowToList(x.df, par.set = getOptProblemParSet(opt.problem), i = 1),
      y = final.points$y, # strip name
      best.ind = final.points$best.ind,
      opt.path = getOptStateOptPath(opt.state),
      resample.results = getOptResultResampleResults(opt.result),
      final.state = getOptStateState(opt.state),
      models = getOptResultStoredModels(opt.result),
      final.opt.state = opt.state,
      control = control
    )
  } else {
    makeS3Obj(c("MBOMultiObjResult", "MBOResult"),
      pareto.front = final.points$pareto.front,
      pareto.set = final.points$pareto.set,
      pareto.inds = final.points$inds,
      opt.path = getOptStateOptPath(opt.state),
      final.state = getOptStateState(opt.state),
      models = getOptResultStoredModels(opt.result),
      final.opt.state = opt.state,
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
#'   \item{pareto.inds [\code{numeric}]}{Indices of the Pareto-optimal points in the opt.path}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.
#'     Includes all evaluated points and additional information as documented in \link{mbo_OptPath}.
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
