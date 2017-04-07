#' @title Sets Refinement Control
#' @description Put another control object into the mbo control object to control the refinement.
#' @template arg_control
#' @param refinement.control [\code{MBOControl}]
#' @param refinement.learner [\code{\link[mlr]{Learner}}]
#' @param fallback [\code{logical(1)}]\cr
#'   In case the refinement.learner (i.e. Kriging) was not able to fit a good regression, should we fall back to the proposal of the original learner and infill.crit? Default is \code{FALSE}.
#'   If this is true fallbacks are indicated by the OptPath column \code{constant.model == TRUE} and empty values for \code{refinement_} infill.crit components
#' @export
setMBOControlRefinement = function(control, refinement.control, refinement.learner, fallback = FALSE) {
  assertClass(control, "MBOControl")
  assertClass(refinement.control, "MBOControl")
  assertClass(refinement.learner, "Learner")
  assertFlag(fallback)

  control$refinement = list(control = refinement.control, learner = refinement.learner, fallback = fallback)
  return(control)
}
