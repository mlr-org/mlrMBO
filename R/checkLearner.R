#' @title Default learner.
#'
#' @description
#' If only numerical and/or integer parameters are present, we use
#' \dQuote{regr.km}, which is kriging / a gaussian process from package DiceKriging.
#' If the objective is noisy, we \code{nugget.estim} is set to \code{TRUE}.
#' If the objective is deterministic, we set nugget = 10^-3 for numerical stabiloity.
#' depending on whether we have noisy observations or not.
#' If a least one parameter is discrete, we use \dQuote{regr.randomForest}.
#'
#' @name mbo_default_learner
NULL

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
