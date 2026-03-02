#' @title Uses efficient global optimization to optimize a function.
#'
#' @description
#' See \link{mbo_parallel} for all parallelization options.
#'
#' @inheritParams mbo
#' @param init.design.points [\code{integer}]\cr
#'   Number of points to generate for initial design via \code{\link[ParamHelpers]{generateDesign}}.
#'   Only used if no design is given in \code{mbo} function.
#'   If for any reason not enough points could be generated, the missing ones are generated randomly.
#'   Default is 4 times the number of all parameters.
#' @param iters [\code{integer}]\cr
#'   Number of sequential optimization steps.
#'   Default is 10.
#'   If a \code{control} object is given, the termination criterion will be overwritten unless \code{iters} is set to \code{NULL}.
#' @examples
#' obj.fun = makeSingleObjectiveFunction(
#'  fn = function(x) x[1]^2 + sin(x[2]), 
#'  par.set = makeNumericParamSet(id = "x", lower = -1, upper = 1, len = 2))
#' res = mboEgo(obj.fun)
#' print(res)
#' plot(res)
#' @export
mboEgo = function(fun, init.design.points = NULL, iters = 10L, design = NULL, learner = NULL, control = NULL, show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {
  
  if (is.null(init.design.points)) {
    init.design.points = sum(getParamLengths(par.set))
  } else {
    assertInt(init.design.points, lower = 1L)
  }

  if (is.null(control)) {
    control = makeMBOControl()
  }

  if (!is.null(iters)) {
    assertInt(iters, lower = 1L)  
    control = setMBOControlTermination(iters = iters)
  }

  if (is.null(design)) {
    design = generateDesign(n = init.design.points, getParamSet(obj.fun), fun = lhs::maximinLHS)
  }

  mbo(fun, design = design, learner = learner, control = control,
  show.info = show.info, more.args = more.args)
}