#' @title Set concept drift options.
#' @description
#' Extends an MBO control object with options for concept drift in the objective function.
#' @template arg_control
#' @param drift.param [\code{character(1)}]\cr
#'   Which parameter determines the drift position we are in?
#' @param drift.function [\code{function}]\cr
#'   Function that returns the position in the drift we are in, depending on the dob.
#' @param window.function [\code{function}]\cr
#'   Function that returns a subset of the OptPathNg. eg. \code{function(x) {tail(x, 10)}}
#' @param learn.drift [\code{logical(1)}]\cr
#'   Should the drift param be included in the surrogate?
#' @param calculate.th.final.point [\code{logical(1)}]\cr
#'   Should the best predicted y be added to the opt.path after each iteration?
#'   Note, that this will be always the theoretical best values of the previous dob!
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlConceptDrift = function(control,
  drift.param = NULL,
  drift.function = NULL,
  window.function = identity,
  learn.drift = FALSE,
  calculate.th.final.point = FALSE) {

  assertClass(control, "MBOControl")
  assertCharacter(drift.param)
  assertFunction(drift.function, args = "dob")
  assertFunction(window.function, args = "x")
  assertFlag(learn.drift)
  assertFlag(calculate.th.final.point)

  control$conceptdrift.drift.param = drift.param %??% control$conceptdrift.drift.param
  control$conceptdrift.drift.function = drift.function %??% control$conceptdrift.drift.function
  control$conceptdrift.window.function = window.function %??% control$conceptdrift.window.function
  control$conceptdrift.learn.drift = learn.drift %??% control$conceptdrift.learn.drift
  control$calculate.th.final.point = calculate.th.final.point %??% control$calculate.th.final.point

  return(control)
}
