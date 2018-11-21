# check and create default learner
checkLearner = function(learner, control, fun) {
  if (missing(learner) || is.null(learner)) {
    learner = makeMBOLearner(control, fun, config = list(show.learner.output = FALSE, on.learner.error = control$on.surrogate.error))
  } else {
    assertClass(learner, "Learner")
    if (!is.null(learner$config$on.learner.error) && learner$config$on.learner.error != control$on.surrogate.error) {
      warningf("mlr setting on.learner.error = '%s' for the surrogate learner '%s' will be overwritten with mlrMBO setting on.surrogate.error = '%s'.", learner$config$on.learner.error, getLearnerId(learner), control$on.surrogate.error)
    }
    learner$config = insert(learner$config, list(on.learner.error = control$on.surrogate.error))
  }
  # so we dont run into problems with focus search et al
  learner$fix.factors.prediction = TRUE
  return(learner)
}
