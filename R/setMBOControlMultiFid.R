#FIXME: document breifly in mbo how multifid is enabled

#' @title Set multi-fidelity options.
#' @description
#' Extends an MBO control object with multi-fidelity specific options.
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
#' @param force.last.level.steps [\code{integer(1)}]
#'   Force an evaluation on the highest fidelity level each i-th step.
#' @template arg_showinfo
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlMultiFid = function(control, param, lvls, costs = NULL, cor.grid.points = NULL,
  force.last.level.steps = NULL, show.info = NULL) {

  assertClass(control, "MBOControl")
  control$multifid.param = coalesce(param, control$multifid.param)
  assertString(control$multifid.param)

  control$multifid.lvls = coalesce(lvls, control$multifid.lvls)
  assertNumeric(control$multifid.lvls, min.len = 2L, any.missing = FALSE)
  if (is.unsorted(control$multifid.lvls))
    stop("MultiFid levels must be sorted!")

  control$multifid = TRUE

  assertNumeric(costs, len = length(control$multifid.lvls))
  control$multifid.costs = costs

  if (!is.null(cor.grid.points))
    cor.grid.points = asInt(cor.grid.points, lower = 2L)
  control$multifid.cor.grid.points = coalesce(cor.grid.points, control$multifid.cor.grid.points, 50L)

  if (!is.null(force.last.level.steps))
    force.last.level.steps = asInt(force.last.level.steps, lower = 0L)
  control$multifid.force.last.level.steps = coalesce(force.last.level.steps, control$multifid.force.last.level.steps, 10L)

  control$multifid.show.info = coalesce(show.info, control$multifid.show.info, FALSE)

  #FIXME check that control$final.evals == 0
  #FIXME check that control$propose.points == 1 ?
  #FIXME check that control$numer.of.targets == 1

  return(control)
}
