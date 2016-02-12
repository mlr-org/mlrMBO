
# check and create default learner
checkLearner = function(learner, par.set, control) {
  if (missing(learner)) {
    if (hasDiscrete(par.set))
      learner = makeLearner("regr.randomForest", predict.type = "se")
    else
      learner = makeLearner("regr.km", covtype = "matern5_2", predict.type = "se",
        nugget.estim = control$noisy)
  } else {
    assertClass(learner, "Learner")
  }
  # so we dont run into problems with focus search et al
  learner$fix.factors.prediction = TRUE

  if (control$multifid) {
    par.set = c(par.set, makeParamSet(
    makeIntegerParam(".multifid.lvl", lower = 1L, upper = length(control$multifid.lvls))))
  }

  return(learner)
}

