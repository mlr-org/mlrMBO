#' @title Automatically generate the right learner for the given problem
#' 
#' @description
#' This is a helper function that generates the right learner for the surrogate, based on criteria of the objective function.
#' For numeric only parameter spaces it returns a Kriging regression learner with the following settings: \cr
#' If the objective function is noisy the nugget effect will be estimated unless \code{nugget.estim = FALSE} is explicitly given in \code{...}.
#' Also \code{jitter} is set to \code{TRUE} to circumvent a problem with DiceKriging where already trained input values produce the exact trained output.
#' For further informations check the \code{$note} slot of the created learner.
#' If the objective function is deterministic we add a small nugget effect to increase numerical stability which prevents crashes of DiceKriging. 
#' The nugget effect is set to 10^-8 * Var(y) on each model training.
#' For mixed parameter spaces the function returns a random forest regression learner.
#' The method to estimate the variance is the standard deviation of the bagged predictions.
#' You can override this setting in \code{...}.
#' 
#' @template arg_control
#' @param fun [\code{smoof_function}] \cr
#'   The same objective function which is also passed to \code{\link{mbo}}.
#' @param ... [any]\cr
#'   Parameters passed to \code{par.vals}.
#'   Will overwrite mlrMBOs recomendations.
#' @return [\code{Learner}]
#' @export
makeMboLearner = function(control, fun, ...) {
  par.vals = list(...)
  if (!hasDiscrete(getParamSet(fun), include.logical = TRUE)) {
    recomended.settings = list(covtype = "matern5_2", multistart = 3)
    par.vals = insert(recomended.settings, par.vals)
    base.learner = makeLearner("regr.km", predict.type = "se")
    if (!control$filter.proposed.points) {
      warningf("filter.proposed.points is not set in the control object. This might lead to the 'leading minor of order ...' error during model fit.")
    }
    if (isNoisy(fun)) {
      par.vals = insert(list(nugget.estim = TRUE, jitter = TRUE), par.vals)
    } else if (is.null(getHyperPars(base.learner)$nugget)) {
      par.vals = insert(list(nugget.stability = 10^-8), par.vals)
    }
    return(setHyperPars(base.learner, par.vals = par.vals))
  } else {
    par.vals = insert(list(se.method = "sd"), par.vals)
    return(makeLearner("regr.randomForest", predict.type = "se", par.vals = par.vals))
  }
}
