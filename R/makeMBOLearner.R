#' @title Generate default learner.
#'
#' @description
#' This is a helper function that generates a default surrogate, based on properties of the objective function and the selected infill criterion.
#'
#' For numeric-only (including integers) parameter spaces without any dependencies :
#' \itemize{
#' \item{A Kriging model \dQuote{regr.km} with kernel \dQuote{matern3_2} is created.}
#' \item{If the objective function is deterministic we add a small nugget effect (10^-8*Var(y),
#'   y is vector of observed outcomes in current design) to increase numerical stability to
#'   hopefully prevent crashes of DiceKriging.}
#' \item{If the objective function is noisy the nugget effect will be estimated with
#'   \code{nugget.estim = TRUE} (but you can override this in \code{...}.}
#'   Also \code{jitter} is set to \code{TRUE} to circumvent a problem with DiceKriging where already
#'   trained input values produce the exact trained output.
#'   For further informations check the \code{$note} slot of the created learner.
#' \item{Instead of the default \code{"BFGS"} optimization method we use rgenoud (\code{"gen"}),
#'   which is a hybrid algorithm, to combine global search based on genetic algorithms and local search
#'   based on gradients.
#'   This may improve the model fit and will produce a constant surrogate model much less frequent.
#'   You can also override this setting in \code{...}.}
#' }
#'
#' For mixed numeric-categorical parameter spaces :
#' \itemize{
#' \item{A random regression forest \dQuote{regr.randomForest} with 500 trees is created.}
#' \item{The standard error of a prediction (if required by the infill criterion) is estimated by computing the jackknife-after-bootstrap, 
#' the mean-squared difference between the prediction made by only using trees which did not contain 
#' said observation and the ensemble prediction. A Monte-Carlo bias correction is applied and, in the 
#' case that this results in a negative variance estimate, the values are truncated at 0.}
#' }
#' If additionally dependencies are in present in the parameter space, an imputation method is added to the random forest:
#' \itemize{
#' \item{If a numeric value is inactive, i.e., missing, it will be imputed by 2 times the maximum of observed values}
#' \item{If a categorical value is inactive, i.e., missing, it will be imputed by the special class label \code{"__miss__"}}
#' }
#'
#' @template arg_control
#' @param fun [\code{smoof_function}] \cr
#'   The same objective function which is also passed to \code{\link{mbo}}.
#' @param ... [any]\cr
#'   Further parameters passed to the constructed learner.
#'   Will overwrite mlrMBO's defaults.
#' @return [\code{Learner}]
#' @aliases mbo_default_learner
#' @export
makeMBOLearner = function(control, fun, ...) {
  assertClass(control, "MBOControl")
  assertClass(fun, "smoof_function")

  ps = getParamSet(fun)
  if (isNumeric(ps, include.int = TRUE) && !hasRequires(ps)) {
    lrn = makeLearner("regr.km", covtype = "matern3_2", optim.method = "gen")
    if (!isNoisy(fun))
      lrn = setHyperPars(lrn, nugget.stability = 10^-8)
    else
      lrn = setHyperPars(lrn, nugget.estim = TRUE, jitter = TRUE)
  } else {
    lrn = makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE)
    if (hasRequires(ps))
      lrn = makeImputeWrapper(lrn, classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__")))
  }
  
  if (control$infill.crit$requires.se)
    lrn = setPredictType(lrn, "se")
  
  lrn = setHyperPars(lrn, ...)
  return(lrn)
}
