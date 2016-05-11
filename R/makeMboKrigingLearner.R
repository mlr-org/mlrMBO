#' @title Automatically generate the right Kriging learner for the given problem
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
makeMboKrigingLearner = function(control, fun, ...) {
  recomended.settings = list(covtype = "matern5_2")
  par.vals = insert(recomended.settings, list(...))
  base.learner = mlr::makeLearner("regr.km", predict.type = "se", par.vals = par.vals)
  if (!control$filter.proposed.points) {
    warningf("filter.proposed.points is not set in the control object. This might lead to the 'leading minor of order ...' error during model fit.")
  }
  if (isNoisy(fun)) {
    learner = mlr::setHyperPars(base.learner, nugget.estim = TRUE)
  } else if (!is.null(mlr::getHyperPars(base.learner)$nugget)) {
    learner = base.learner
  } else {
    base.learner = mlr::setHyperPars(base.learner)
    learner = mlr:::makeBaseWrapper(
    id = "mboKriging",
    type = base.learner$type,
    next.learner = base.learner,
    learner.subclass = "MboKrigingWrapper",
    model.subclass = "MboKrigingModel"
    )
  }
  return(learner)
}

#' @export
trainLearner.MboKrigingWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  .task$weights = .weights
  .task = mlr::subsetTask(.task, .subset)
  y = mlr::getTaskData(.task, target.extra = TRUE)$target
  .learner$next.learner = mlr::setHyperPars(.learner$next.learner, par.vals = list(nugget = 10^-8 * var(y)))
  m = mlr::train(.learner$next.learner, .task, weights = .task$weights)
  mlr:::makeChainModel(next.model = m, cl = "MboKrigingModel")
}
