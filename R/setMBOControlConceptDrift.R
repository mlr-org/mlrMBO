#' @title Set concept drift options.
#' @description
#' Extends an MBO control object with options for concept drift in the objective function.
#' @template arg_control
#' @param drift.function [\code{function}]\cr
#'   Function that returns the position in the drift we are in, depending on the dob.
#' @param window.function [\code{function}]\cr
#'   Function that returns a subset of the OptPathNg. eg. \code{function(x) {tail(x, 10)}}
#' @param learn.drift [\code{logical(1)}]\cr
#'   Should the drift param be included in the surrogate?
#' @param calculate.th.final.point [\code{logical(1)}]\cr
#'   Should the best predicted y be added to the opt.path after each iteration?
#'   Note, that this will be always the theoretical best values of the previous dob!
#' @param fix.constant.model [\code{logical(1)}]\cr
#'   If \code{TRUE} and the model gives constant predictions we will propose the x that led to the best y and not ask the model for predictions.
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlConceptDrift = function(control,
  drift.function = identity,
  window.function = identity,
  learn.drift = FALSE,
  calculate.th.final.point = FALSE,
  fix.constant.model = FALSE) {

  assertClass(control, "MBOControl")
  assertFunction(drift.function, args = "dob")
  assertFunction(window.function, args = "x")
  assertFlag(learn.drift)
  assertFlag(calculate.th.final.point)
  assertFlag(fix.constant.model)

  control$conceptdrift.drift.function = drift.function %??% control$conceptdrift.drift.function
  control$conceptdrift.window.function = window.function %??% control$conceptdrift.window.function
  control$conceptdrift.learn.drift = learn.drift %??% control$conceptdrift.learn.drift
  control$calculate.th.final.point = calculate.th.final.point %??% control$calculate.th.final.point
  control$fix.constant.model = fix.constant.model %??% control$fix.constant.model
  # if we have window.function == identity and learn.drift == FALSE we allow to use other final methods!
  if((!identical(identity, window.function) || learn.drift) && !control$final.method %in% c("best.predicted", "predict")){
    warningf("final.method = %s does not make sense for CD.", control$final.method)
  }

  return(control)
}
