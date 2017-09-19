#' @title Set concept drift options.
#' @description
#' Extends an MBO control object with options for concept drift in the objective function.
#' @template arg_control
#' @param drift.param [\code{character(1)}]\cr
#'   Which parameter determines the drift position we are in?
#' @param drift.function [\code{function}]\cr
#'   Function that returns the position in the drift we are in, depending on the dob.
#' @param window.function [\code{function}]\cr
#'   Function that returns a subset of the OptPathNg. eg. \code{function(op) {r = op$clone(); r$data = tail(r$data); r}}
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlConceptDrift = function(control,
  drift.param = NULL,
  drift.function = NULL,
  window.function = NULL) {

  assertClass(control, "MBOControl")
  assertCharacter(drift.param)
  assertFunction(drift.function, args = "dob")
  assertFunction(window.function, args = "op")

  control$conceptdrift.drift.param = drift.param %??% control$conceptdrift.drift.param
  control$conceptdrift.drift.function = drift.function %??% control$conceptdrift.drift.function
  control$conceptdrift.window.function = window.function %??% control$conceptdrift.window.function

  return(control)
}
