#' @title Optimizes a function with sequential model based optimization.
#'
#' @description
#' See \link{mbo_parallel} for all parallelization options.
#'
#' @param fun [\code{smoof_function}]\cr
#'   Fitness function to optimize.
#'   For one dimensional target functions you can obtain a \code{smoof_function} by using \code{\link[smoof]{makeSingleObjectiveFunction}}.
#'   For multi dimensional functions use \code{\link[smoof]{makeMultiObjectiveFunction}}.
#'   It is possible to return even more information which will be stored
#'   in the optimization path. To achieve this, simply append the attribute \dQuote{extras}
#'   to the return value of the target function. This has to be a named list of scalar values.
#'   Each of these values will be stored additionally in the optimization path.
#' @param design [\code{data.frame}]\cr
#'   Initial design as data frame.
#'   If the y-values are not already present in design, mbo will evaluate the points.
#'   If the parameters have corresponding trafo functions, the design must not be transformed before it is passed!
#'   Functions to generate designs are available in \code{ParamHelpers}: \code{\link[ParamHelpers]{generateDesign}}, \code{\link[ParamHelpers]{generateGridDesign}}, \code{\link[ParamHelpers]{generateRandomDesign}}.
#'   Default is \code{NULL}, which means \code{\link[ParamHelpers]{generateDesign}} is called and a design of size 4 times number of all parameters is created
#'   The points are drawn via \code{\link[lhs]{maximinLHS}} to maximize the minimal distance between design points.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner from mlr, which is used as a surrogate to model our fitness function.
#'   If \code{NULL} (default), the default learner is determined as described here: \link{mbo_default_learner}.
#' @template arg_control
#' @template arg_showinfo
#' @param more.args [list]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{\link{MBOSingleObjResult}} | \code{\link{MBOMultiObjResult}}]
#' @export
#' @examples
#' # simple 2d objective function
#' obj.fun = makeSingleObjectiveFunction(
#'  fn = function(x) x[1]^2 + sin(x[2]),
#'  par.set = makeNumericParamSet(id = "x", lower = -1, upper = 1, len = 2)
#' )
#'
#' # create base control object
#' ctrl = makeMBOControl()
#'
#' # do three MBO iterations
#' ctrl = setMBOControlTermination(ctrl, iters = 3L)
#'
#' # use 500 points in the focussearch (should be sufficient for 2d)
#' ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 500)
#' # create initial design
#' des = generateDesign(n = 5L, getParamSet(obj.fun), fun = lhs::maximinLHS)
#'
#' # start mbo
#' res = mbo(obj.fun, design = des, control = ctrl)
#'
#' print(res)
#' \dontrun{
#' plot(res)
#' }
mbo = function(fun, design = NULL, learner = NULL, control,
  show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {

  assertClass(fun, "smoof_function")
  par.set = getParamSet(fun)
  n.params = sum(getParamLengths(par.set))
  control$noisy = isNoisy(fun)
  control$minimize = shouldBeMinimized(fun)
  assertFlag(show.info)
  if (is.null(design))
    design = generateDesign(n.params * 4L, par.set, fun = lhs::maximinLHS)
  else
    assertDataFrame(design, min.rows = 1L, min.cols = 1L)
  learner = checkLearner(learner, par.set, control, fun)
  control = checkStuff(fun, par.set, design, learner, control)
  control$infill.crit = initCrit(control$infill.crit, fun, design, learner, control)

  loadPackages(control)

  # generate an OptProblem which gathers all necessary information to define the optimization problem in one environment.
  opt.problem = makeOptProblem(
    fun = fun,
    design = design,
    learner = learner,
    control = control,
    show.info = show.info,
    more.args = more.args)

  # we call the magic mboTemplate where everything happens
  final.opt.state = mboTemplate(opt.problem)

  mboFinalize2(final.opt.state)
}
