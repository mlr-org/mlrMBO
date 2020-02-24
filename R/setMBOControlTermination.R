#' @title Set termination options.
#'
#' @description
#' Extends an MBO control object with infill criteria and infill optimizer options.
#'
#' @template arg_control
#' @param iters [\code{integer(1)}]\cr
#'   Number of sequential optimization steps.
#' @param time.budget [\code{numeric(1)} | NULL]\cr
#'   Running time budget in seconds. Note that the actual mbo run can take more time since
#'   the condition is checked after each iteration.
#'   The default \code{NULL} means: There is no time budget.
#' @param exec.time.budget [\code{numeric(1)} | NULL]\cr
#'   Execution time (time spent executing the function passed to \code{mbo})
#'   budget in seconds. Note that the actual mbo run can take more time since
#'   the condition is checked after each iteration.
#'   The default \code{NULL} means: There is no execution time budget.
#' @param target.fun.value [\code{numeric(1)}] | NULL]\cr
#'   Termination criterion for single-objective optimization: Stop if a function evaluation
#'   is better than this given target.value.
#'   The default \code{NULL} means: The function value won't be taken into account for termination.
#' @param max.evals [\code{integer(1) | NULL}]\cr
#'   Maximal number of function evaluations.
#'   The default \code{NULL} means: The total number of evaluations won't be taken into account for termination.
#' @param more.termination.conds [\code{list}]\cr
#'   Optional list of termination conditions. Each condition needs to be a function
#'   of a single argument \code{opt.state} of type \code{\link{OptState}} and should
#'   return a list with the following elements:
#'   \describe{
#'     \item{term [\code{logical(1)}]}{Logical value indicating whether the
#'     termination condition is met.}
#'     \item{message [\code{character(1)}]}{Termination message. At the moment we just allow \code{term.custom}.}
#'   }
#' @param use.for.adaptive.infill [\code{character(1)}|NULL]\cr
#'   Which termination criterion should determine the progress that is used for adaptive infill criteria like [\code{\link{makeMBOInfillCritAdaCB}}].
#'   The default is \code{NULL} which means, that the first supplied argument is taken, following the order of the function signature.
#'   Other values can be \code{"iters"}, \code{"time.budget"}, etc.\cr
#'   If you want to to use it together with a criterion you supplied in \code{more.termination.conds}, \code{more.termination.conds} has to be a named list and the function further has to return a list element \code{progress} with values between 0 and 1.
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
#' @examples
#' fn = smoof::makeSphereFunction(1L)
#' ctrl = makeMBOControl()
#'
#' # custom termination condition (stop if target function value reached)
#' # We neglect the optimization direction (min/max) in this example.
#' yTargetValueTerminator = function(y.val) {
#'   force(y.val)
#'   function(opt.state) {
#'     opt.path = opt.state$opt.path
#'     current.best = getOptPathEl(opt.path, getOptPathBestIndex((opt.path)))$y
#'     term = (current.best <= y.val)
#'     message = if (!term) NA_character_ else sprintf("Target function value %f reached.", y.val)
#'     return(list(term = term, message = message))
#'   }
#' }
#'
#' # assign custom termination condition
#' ctrl = setMBOControlTermination(ctrl, more.termination.conds = list(yTargetValueTerminator(0.05)))
#' res = mbo(fn, control = ctrl)
#' print(res)
setMBOControlTermination = function(control,
  iters = NULL, time.budget = NULL, exec.time.budget = NULL, target.fun.value = NULL, max.evals = NULL, more.termination.conds = list(), use.for.adaptive.infill = NULL) {

  assertClass(control, "MBOControl")
  assertList(more.termination.conds)
  assertCharacter(use.for.adaptive.infill, null.ok = TRUE)

  stop.conds = more.termination.conds

  if (is.null(iters) && is.null(time.budget) && is.null(exec.time.budget) && is.null(max.evals) && length(stop.conds) == 0L) {
    stopf("You need to specify a maximal number of iteration, a time budget or at least
      one custom termination condition, but you provided neither.")
  }

  if (!is.null(iters)) {
    stop.conds = c(stop.conds, iters = makeMBOTerminationMaxIter(iters))
  }

  if (!is.null(time.budget)) {
    stop.conds = c(stop.conds, time.budget = makeMBOTerminationMaxBudget(time.budget))
  }

  if (!is.null(exec.time.budget)) {
    stop.conds = c(stop.conds, exec.time.budget = makeMBOTerminationMaxExecBudget(exec.time.budget))
  }

  if (!is.null(target.fun.value)) {
    if (control$n.objectives > 1L)
      stop("Specifying target.fun.value is only useful in single-objective optimization.")
    stop.conds = c(stop.conds, target.fun.value = makeMBOTerminationTargetFunValue(target.fun.value))
  }

  if (!is.null(max.evals)) {
    stop.conds = c(stop.conds, max.evals = makeMBOTerminationMaxEvals(max.evals))
  }


  # sanity check termination conditions
  lapply(stop.conds, function(stop.on) {
    assertFunction(stop.on, args = "opt.state")
  })

  # sanity check, whether use.for.adaptive.infill was set to an active criterion
  if (is.null(use.for.adaptive.infill)) {
    use.for.adaptive.infill = names(stop.conds)[1]
  } else {
    assertSubset(use.for.adaptive.infill, names(stop.conds))
  }

  control$stop.conds = stop.conds

  # store stuff in control object since it is needed internally
  control$iters = coalesce(iters, control$iters, Inf)
  control$time.budget = coalesce(time.budget, control$time.budget, Inf)
  control$exec.time.budget = coalesce(exec.time.budget, control$exec.time.budget, Inf)
  control$max.evals = coalesce(max.evals, Inf)
  control$use.for.adaptive.infill = use.for.adaptive.infill

  return(control)
}
