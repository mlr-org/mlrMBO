checkLearner = function(learner, par.set, control, ...) {

  if (missing(learner)) {
    if (hasDiscrete(par.set)) {
      learner = makeLearner("regr.randomForest", predict.type = "se")
    } else {
      learner = makeLearner("regr.km", covtype = "matern5_2", predict.type = "se",
        nugget.estim = control$noisy, ...)
    }
  } else {
    checkArg(learner, "Learner")
  }
  # FIXME: I am unsure wether we should do this, but otherwise RF sucks
  # if it is a good idea it is not not general enuff
  if (inherits(learner, "regr.randomForest"))
    learner = setHyperPars(learner, fix.factors = TRUE)
  return(learner)
}

