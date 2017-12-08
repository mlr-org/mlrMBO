# check and create default learner
checkLearner = function(learner, control, fun) {
  if (missing(learner) || is.null(learner)) {
    learner = makeMBOLearner(control, fun, config = list(show.learner.output = FALSE, on.learner.error = control$on.surrogate.error))
  } else {
    assertClass(learner, "Learner")
    learner$config = insert(learner$config, list(on.learner.error = control$on.surrogate.error))
  }
  # so we dont run into problems with focus search et al
  learner$fix.factors.prediction = TRUE
  return(learner)
}
