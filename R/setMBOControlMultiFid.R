#FIXME: document breifly in mbo how multifid is enabled

#' @title Extends mbo control object with multiFid-algorithm specific options.
#'
#' @template arg_control
#' @param param [\code{character(1)}]\cr
#'   The name of the parameter which increases the performance but also calculation costs.
#' @param lvls [\code{numeric}]\cr
#'   The values of the param the learner should be trained with, in ascending order.
#' @param cor.grid.points [\code{integer(1)}]\cr
#'   Numbers of points used to calculate the correlation between the different levels of
#'   the \code{param}.
#' @param costs [\code{numeric}]\cr
#'   Vector defining the cost for each level.
#'   Default is \code{NULL} which means that the cost will be predicted by a model build on the \code{exec.time} from the so far evaluated points.
#' @param force.last.level.evals [\code{integer(1)}]
#'   How many evaluations should be done on the last value of fid.param?
#' @param last.level.crit [\code{character(1)}]\cr
#'   How should infill points be rated. Possible parameter values are:
#'   \dQuote{mean}: Mean response.
#'   \dQuote{ei}: Expected improvement.
#'   \dQuote{aei}: Augmented expected improvement.
#'   \dQuote{eqi}: Expected quantile improvement.
#'   \dQuote{lcb}: Lower confidence bound.
#'   \dQuote{multiFid}: Multifidelity: Expected improvement on different levels of the perfomance parameter defined in the \code{MBOMultiFidControl}.
#'   Alternatively, you may pass a function name as string.
#' @param generate.plot.data [\code{boolean(1)}]
#'   Should plot data be generated? Default ist \code{TRUE}.
#' @template arg_showinfo
#' @return [\code{\link{MBOControl}}].
#' @note See the other setMBOControl... functions and \code{makeMBOControl} for referenced arguments.
#' @seealso makeMBOControl
#' @export
setMBOControlMultiFid = function(control, param, lvls, costs = NULL, cor.grid.points = NULL,
  force.last.level.evals = NULL, eval.lower = NULL, last.level.crit = NULL, generate.plot.data = TRUE, show.info = NULL) {

  assertClass(control, "MBOControl")
  control$multifid.param = coalesce(param, control$multifid.param)
  assertString(control$multifid.param)

  control$multifid.lvls = coalesce(lvls, control$multifid.lvls)
  assertNumeric(control$multifid.lvls, min.len = 2L, any.missing = FALSE)
  if (is.unsorted(control$multifid.lvls))
    stop("MultiFid levels must be sorted!")

  control$multifid.eval.lower = coalesce(eval.lower, control$multifid.eval.lower, FALSE)
  assertLogical(control$multifid.eval.lower)

  control$multifid = TRUE

  if (!is.null(costs)) {
    assertNumeric(costs, len = length(control$multifid.lvls))
  }
  control$multifid.costs = costs

  if (!is.null(cor.grid.points))
    cor.grid.point = asInt(cor.grid.points, lower = 2L)
  control$multifid.cor.grid.points = coalesce(cor.grid.points, control$multifid.cor.grid.points, 50L)

  if (!is.null(force.last.level.evals))
    force.last.level.evals = asInt(force.last.level.evals, lower = 0L)
  control$multifid.force.last.level.evals = coalesce(force.last.level.evals, control$multifid.force.last.level.evals, 10L)

  control$multifid.last.level.crit = coalesce(last.level.crit, control$multifid.last.level.crit, "ei")
  assertChoice(control$multifid.last.level.crit, choices = getSupportedInfillCritFunctions())

  control$multifid.generate.plot.data = coalesce(generate.plot.data, control$multifid.generate.plot.data, TRUE)
  # FIXME: This following line is maybe not needed anymore. proposePoints() might not work now for multiFid control objects.
  control$multifid.show.info = coalesce(show.info, control$multifid.show.info, FALSE)

  return(control)
}
