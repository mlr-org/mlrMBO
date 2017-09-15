#' @title Set concept drift options.
#' @description
#' Extends an MBO control object with options for concept drift in the objective function.
#' @template arg_control
#' @param drift.param [\code{character(1)}]\cr
#'   Which parameter determines the drift position we are in?
#' @param drift.function [\code{function}]\cr
#'   Function that returns the position in the drift we are in, depending on the dob.
#' @param window.width [\code{integer(1)}]\cr
#'   Width (in dobs) of the window that will be used to train the surrogate.
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlConceptDrift = function(control,
  drift.param = NULL,
  drift.function = NULL,
  window.width = NULL) {

  assertClass(control, "MBOControl")
  assertCharacter(drift.param)
  assertFunction(drift.function, args = "dob")
  assertIntegerish(window.width)

  control$conceptdrift.drift.param = coalesce(drift.param, control$conceptdrift.drift.param)
  control$conceptdrift.drift.function = coalesce(drift.function, control$conceptdrift.drift.function)
  control$conceptdrift.window.width = coalesce(window.width, control$conceptdrift.window.width)

  return(control)
}
