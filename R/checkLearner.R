# check and create default learner
checkLearner = function(learner, par.set, control, fun) {
  if (missing(learner) || is.null(learner)) {
    learner = makeMBOLearner(control, fun)
  } else {
    assertClass(learner, "Learner")
  }
  # so we dont run into problems with focus search et al
  learner$fix.factors.prediction = TRUE
  return(learner)
}
