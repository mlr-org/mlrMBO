#' Extends mbo control object with multiFid-algorithm specific options.
#'
#' @param control [\code{MBOControl}]\cr
#'   MBO control object.
#' @param multifid.param [\code{character(1)}]\cr
#'   The name of the parameter which increases the performance but also calculation costs. Has to belong to a discrete Parameter.
#' @param multifid.param.lvls [\code{numeric}]\cr
#'   The values of the multifid.param the learner should be trained with.
#' @param multifid.cor.grid.points [\code{integer(1)}]\cr
#'   Numbers of points used to calculate the correlation between the different levels of the \code{multiFid.fid.param}.
#' @param multifid.costs [\code{function}]\cr
#'   Vektorized (?) cost function with the params \code{cur} and \code{last}.
#' @param force.last.level.evals [\code{integer(1)}]
#'   How many evaluations should be done on the last value of fid.param.
#' @return [\code{\link{MBOControl}}].
#' @note See the other setMBOControl... functions and \code{makeMBOControl} for referenced arguments.
#' @seealso makeMBOControl
#' @export
setMBOControlMultiFid = function(control,
                                 multifid.param,
                                 multifid.lvls,
                                 multifid.costs = NULL,
                                 multifid.cor.grid.points = 10L,
                                 multifid.force.last.level.evals = 10L){
  assertClass(control, "MBOControl")
  assertCharacter(multifid.param, len = 1L, any.missing = FALSE)
  assertNumeric(multifid.lvls, any.missing = FALSE, min.len = 1L)
  if(!is.null(multifid.costs)){
    assertFunction(multifid.costs, args = c("cur", "last"), ordered = TRUE)
  } else {
    multifid.costs = function(cur, last) (last / cur)^2
  }
  assertInt(multifid.cor.grid.points, lower=2L)
  assertInt(multifid.force.last.level.evals, lower=0L)

  # extend control object
  control$multifid.param = multifid.param
  control$multifid.lvls = multifid.lvls
  control$multifid.costs = multifid.costs
  control$multifid.cor.grid.points = multifid.cor.grid.points
  control$multifid.force.last.level.evals = multifid.force.last.level.evals

  return(control)
}

