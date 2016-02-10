#' @title Extends mbo control object with infill criteria and infill optimizer options.
#'
#' @template arg_control
#' @param iters [\code{integer(1)}]\cr
#'   Number of sequential optimization steps.
#'   Default is 10.
#' @param time.budget [\code{integer(1)} | NULL]\cr
#'   Running time budget in seconds. Note that the actual mbo run can take more time since
#'   the condition is checked after each iteration.
#' @param exec.time.budget [\code{integer(1)} | NULL]\cr
#'   Execution time (time spent executing the function passed to \code{mbo})
#'   budget in seconds. Note that the actual mbo run can take more time since
#'   the condition is checked after each iteration.
#' @param target.fun.value [\code{numeric(1)}] | NULL]\cr
#'   Stopping criterion for single crit optimization: Stop if a function evaluation
#'   is better than this given target.value.
#' @param ... [\code{function}]\cr
#'   Optional list of termination functions. The function needs to accept an \code{\link{OptState}}
#'   object \code{opt.state} and should return a list with the following elements:
#'   \describe{
#'     \item{message [\code{character(1)}]}{Stopping message.}
#'     \item{condition [\code{logical(1)}]}{Logical value indivating whether the
#'     stopping condition is met.}
#'   }
#' @return [\code{\link{MBOControl}}].
#' @note See the other setMBOControl... functions and \code{makeMBOControl} for referenced arguments.
#' @seealso makeMBOControl
#' @export
setMBOControlTermination = function(control
  iters = 10L, time.budget = NULL, exec.time.budget = NULL, target.fun.value = NULL, ...) {

  stop.conds = list(...)

  if (is.null(iters) && is.null(time.budget) && is.null(exec.time.budget) && length(stop.conds) == 0L) {
    stopf("You need to specify a maximal number of iteration, a time budget or at least
      one custom stopping condition, but you provided neither.")
  }

  if (!is.null(iters)) {
    stop.conds = c(stop.conds, makeMBOMaxIterTermination(max.iter))
  }

  if (!is.null(time.budget)) {
    stop.conds = c(stop.conds, makeMBOTimeBudgetTermination(time.budget))
  }

  if (!is.null(exec.time.budget)) {
    stop.conds = c(stop.conds, makeMBOTimeBudgetTermination(exec.time.budget))
  }

  if (control$number.of.targets > 1L & !is.null(target.fun.value)) {
    stop("Specifying target.fun.value is only useful in single crit optimization.")
  } else {
    stop.conds = c(stop.conds, makeMBOTargetFunValueTermination(target.fun.value))
  }

  sapply(stop.conds, function(stop.on) {
    assertFunction(stop.on, args = "opt.state")
  })

  control$stop.conds = stop.conds

  return(control)
}

makeMBOMaxIterTermination = function(max.iter) {
  assertCount(max.iter, na.ok = FALSE, positive = TRUE)
  force(max.iter)
  function(opt.state) {
    iter = getOptStateLoop(opt.state)
    term = iter > max.iter
    message = if (!term) NA_character_ else sprintf("Maximum number of iterations %i reached with.", max.iter, iter)
    return(list(term = term, message = message))
  )
}

makeMBOTargetFunValueTermination = function(target.fun.value) {
  assertNumber(target.fun.value, na.ok = FALSE)
  force(target.fun.value)
  function(opt.state) {
    control = getOptStateControl(opt.state)
    opt.path = getOptStateOptPath(opt.state)
    opt.dir = if (control$minimize) 1L else -1L
    current.best = getOptPathEl(opt.path, getOptPathBestIndex((opt.path)))$y
    term = (current.best * opt.dir <= target.fun.value * opt.dir)
    message = if (!term) NA_character_ else sprintf("Target function value %f reached.", target.fun.value)
    return(list(term = term, message = message))
  )
}

makeMBOMaxBudgetTermination = function(time.budget) {
  assertNumber(time.budget, na.ok = FALSE)
  force(time.budget)
  function(opt.state) {
    time.used = as.numeric(getOptStateTimeUsed(opt.state), units = "secs")
    term = (time.used > time.budget)
    if (term) {
      showInfo(show.info, "time.budget %i reached with %.1f", time.budget, time.used)
    }
    message = if (!term) NA_character_ else sprintf("Time budged %f reached.", time.budget)
    return(list(term = term, message = message))
  )
}

