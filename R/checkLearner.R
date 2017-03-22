# check and create default learner
checkLearner = function(learner, par.set, control, fun, show.info) {
  if (missing(learner) || is.null(learner)) {
    if (!show.info) {
      config = list(show.learner.output = FALSE)
    } else {
      config = list()
    }
    learner = makeMBOLearner(control, fun, config = config)
  } else {
    assertClass(learner, "Learner")
  }
  # so we dont run into problems with focus search et al
  learner$fix.factors.prediction = TRUE
  return(learner)
}
