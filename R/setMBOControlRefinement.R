#' @export
setMBOControlRefinement = function(control, refinement.control, refinement.learner) {
  assertClass(control, "MBOControl")
  assertClass(refinement.control, "MBOControl")
  assertClass(refinement.learner, "Learner")

  control$refinement = list(control = refinement.control, learner = refinement.learner)
  return(control)
}
