#' @title Optimizes a function with sequential model based optimization.
#'
#' @description
#' See \link{mbo_parallel} for all parallelization options.
#'
#' @param fun [\code{function(x, ...)}]\cr
#'   Fitness function to minimize. The first argument has to be a list of values.
#'   The function has to return a single numerical value.
#'   In fact it is possible to return even more information which will be stored
#'   in the optimization path. To achieve this, simply append the attribute \dQuote{extras}
#'   to the return value of the target function. This has to be a named list of scalar values.
#'   Each of these values will be stored additionally in the optimization path.
#' @param design [\code{data.frame}]\cr
#'   Initial design as data frame. If the y-values are not already present in design,
#'   mbo will evaluate the points.
#'   If the parameters have corresponding trafo functions,
#'   the design must not be transformed before it is passed!
#'   Default is \code{NULL}, which means \code{\link{generateDesign}} is called and a design
#'   of size 4 times number of all parameters is created.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Default is mlr learner \dQuote{regr.km}, which is kriging from package
#'   DiceKriging, if all parameters are numeric. \code{nugget.estim} is set
#'   to \code{TRUE} depending on whether we have noisy observations or not.
#'   If a least one parameter is discrete the mlr learner \dQuote{regr.randomForest}
#'   from package RandomForest is used as the default.
#' @template arg_control
#' @template arg_showinfo
#' @param more.args [list]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{\link{MBOSingleObjResult}} | \code{\link{MBOMultiObjResult}}]
#' @export
mbo = function(fun, design, learner, control,
  show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {

  assertClass(fun, "smoof_function")
  par.set = smoof::getParamSet(fun)
  n.params = sum(getParamLengths(par.set))
  control$noisy = isNoisy(fun)
  control$minimize = shouldBeMinimized(fun)
  assertFlag(show.info)
  if (is.null(design))
    design = generateDesign(n.params * 4L, par.set)
  else
    assertDataFrame(design, min.rows = 1L, min.cols = 1L)
  learner = checkLearner(learner, par.set, control)
  control = checkStuff(fun, par.set, design, learner, control)

  loadPackages(control)

  # generate an OptProblem which gathers all necessary information to define the optimization problem in one environment.
  opt.problem = makeOptProblem(
    fun = fun,
    par.set = par.set,
    design = design,
    learner = learner,
    control = control,
    show.info = show.info,
    more.args = more.args)

  # we call the magic mboTemplate where everything happens
  final.opt.state = mboTemplate(opt.problem)

  mboFinalize2(final.opt.state)
}
