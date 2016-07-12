#' @title Automatically generate the right learner for the given problem
#' 
#' @description
#' Based on in the function is noisy or not this function generates a kriging learner whichs settings are the right ones for mbo.
#' Additionally it determines the nugget for each model fit based on the variance of the target function.
#' 
#' @template arg_control
#' @param fun [\code{smoof_function}] \cr
#'   The same objective function which is also passed to \code{\link{mbo}}.
#' @param ... [any]\cr
#'   Parameters passed to \code{\link[DiceKriging]{km}}.
#'   Will overwrite mlrMBOs recomendations.
#' @return [\code{Learner}]
#' @export
makeMboLearner = function(control, fun, ...) {
  par.vals = list(...)
  if (!hasDiscrete(getParamSet(fun), include.logical = TRUE)) {
    recomended.settings = list(covtype = "matern5_2", multistart = 3)
    par.vals = insert(recomended.settings, list(...))
    base.learner = makeLearner("regr.km", predict.type = "se")
    if (!control$filter.proposed.points) {
      warningf("filter.proposed.points is not set in the control object. This might lead to the 'leading minor of order ...' error during model fit.")
    }
    if (is.null(par.vals$nugget.estim) && isNoisy(fun)) {
      learner = setHyperPars(base.learner, nugget.estim = TRUE)
    } else if (is.null(getHyperPars(base.learner)$nugget) && is.null(getHyperPars(base.learner)$nugget.stability)) {
      learner = setHyperPars(base.learner, nugget.stability = 10^-8)
    } else {
      learner = setHyperPars(base.learner, par.vals)
    }
  } else {
    learner = makeLearner("regr.randomForest", predict.type = "se", par.vals = par.vals)
  }
  return(learner)
}