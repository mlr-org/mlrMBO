#' @title Default learner used in mbo.
#'
#' @description
#' If only numerical and/or integer parameters are present, we use mlr learner
#' \dQuote{regr.km}, which is kriging / a gaussian process from package DiceKriging, with
#' the covariance kernel \code{covtype = "matern5_2"}.
#' If the objective is noisy, we set \code{nugget.estim} to \code{TRUE}.
#' If the objective is deterministic, we set \code{nugget = 10^-4} for numerical stability.
#'
#' If a least one parameter is discrete, we use mlr learner \dQuote{regr.randomForest}, a random
#' regression forest from package randomForest. The default se estimator of the forest in mlr is used.
#'
#' @name mbo_default_learner
NULL

# check and create default learner
checkLearner = function(learner, par.set, control, fun) {
  if (missing(learner) || is.null(learner)) {
    if (!hasDiscrete(par.set, include.logical = TRUE)) {
      learner = makeMboKrigingLearner(control, fun)
    } else {
      learner = makeLearner("regr.randomForest", predict.type = "se")
    }
  } else {
    assertClass(learner, "Learner")
  }
  # so we dont run into problems with focus search et al
  learner$fix.factors.prediction = TRUE
  return(learner)
}
