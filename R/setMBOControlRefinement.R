#' @title Sets Refinement Control
#' @description Put another control object into the mbo control object to control the refinement.
#' @template arg_control
#' @param refinement.control [\code{MBOControl}]
#' @param refinement.learner [\code{\link[mlr]{Learner}}]
#' @export
setMBOControlRefinement = function(control, refinement.control, refinement.learner) {
  assertClass(control, "MBOControl")
  assertClass(refinement.control, "MBOControl")
  assertClass(refinement.learner, "Learner")

  control$refinement = list(control = refinement.control, learner = refinement.learner)
  return(control)
}
