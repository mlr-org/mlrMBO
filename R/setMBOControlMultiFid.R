#FIXME: document breifly in mbo how multifid is enabled

#' @title Extends mbo control object with multiFid-algorithm specific options.
#'
#' @template arg_control
#' @param param [\code{character(1)}]\cr
#'   The name of the parameter which increases the performance but also calculation costs.
#'   Must be a discrete parameter.
#' @param lvls [\code{numeric}]\cr
#'   The values of the param the learner should be trained with.
#' @param cor.grid.points [\code{integer(1)}]\cr
#'   Numbers of points used to calculate the correlation between the different levels of
#'   the \code{param}.
#' @param costs [\code{function}]\cr
#'   Vektorized (?) cost function with the params \code{cur} and \code{last}.
#' @param force.last.level.evals [\code{integer(1)}]
#'   How many evaluations should be done on the last value of fid.param?
#' @param eval.lower [\code{boolean(1)}]\cr
#'   For each evaluation, evaluate the lower fidelity levels as well. Default is \code{FALSE}.
#' @return [\code{\link{MBOControl}}].
#' @note See the other setMBOControl... functions and \code{makeMBOControl} for referenced arguments.
#' @seealso makeMBOControl
#' @export
setMBOControlMultiFid = function(control, param, lvls, costs = NULL, cor.grid.points = NULL,
  force.last.level.evals = NULL, eval.lower = NULL, show.info = NULL) {

  assertClass(control, "MBOControl")
  control$multifid.param = coalesce(param, control$multifid.param)
  assertString(control$multifid.param)

  control$multifid.lvls = coalesce(lvls, control$multifid.lvls)
  assertNumeric(control$multifid.lvls)

  control$multifid.eval.lower = coalesce(eval.lower, control$multifid.eval.lower, FALSE)
  assertLogical(control$multifid.eval.lower)

  control$multifid = TRUE

  if (!is.null(costs)){
    assertFunction(costs, args = c("cur", "last"), ordered = TRUE)
  } else {
    costs = function(cur, last) (last / cur)^2
  }
  control$multifid.costs = costs

  if (!is.null(cor.grid.points)) {
    cor.grid.point = asInt(cor.grid.points, lower = 2L) 
  }
  control$multifid.cor.grid.points = coalesce(cor.grid.points, control$multifid.cor.grid.points, 10L)

  if (!is.null(force.last.level.evals)) {
    force.last.level.evals = asInt(force.last.level.evals, lower = 0L)
  }
  control$multifid.force.last.level.evals = coalesce(force.last.level.evals, control$multifid.force.last.level.evals, 10L)

  # FIXME: This following line is maybe not needed anymore. proposePoints() might not work now for multiFid control objects. 
  control$multifid.show.info = coalesce(show.info, control$multifid.show.info, FALSE)

  return(control)
}

