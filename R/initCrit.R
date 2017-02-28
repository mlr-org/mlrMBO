#' @title Initialize an MBO infill criterion.
#'
#' @description
#'   Some infill criteria have parameters that are dependent on values in the parameter set, design,
#'   used learner or other control settings.
#'   To actually set these default values, this function is called, which returns a fully
#'   initialized [\code{\link{MBOInfillCrit}}].
#'   This function is mainly for internal use. If a custom infill criterion is created, it may be
#'   required to create a seperate method \code{initCrit.InfillCritID} where \code{ID} is the
#'   \code{id} of the custom \link{MBOInfillCrit}.
#'
#' @param crit [\code{\link{MBOInfillCrit}}]\cr
#'   Uninitialized infill criterion.
#' @param fun [\code{smoof_function}]\cr
#'   Fitness function to optimize.
#' @param design
#'   Sampling plan.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner from mlr, which is used as a surrogate to model our fitness function.
#' @param control [\code{\link{MBOControl}}]\cr
#'   MBO control object.
#' @return [\code{\link{MBOInfillCrit}}]
#' @export
initCrit = function(crit, fun, design, learner, control) {
  UseMethod("initCrit")
}

#' @export
initCrit.default = function(crit, fun, design, learner, control) {
  crit = initCritOptDirection(crit, fun)
  return(crit)
}

#' @export
initCrit.InfillCritCB = function(crit, fun, design, learner, control) {
  cb.lambda = crit$params$cb.lambda
  if (is.null(cb.lambda))
    cb.lambda = ifelse(isSimpleNumeric(getParamSet(fun)), 1, 2)
  crit = makeMBOInfillCritCB(cb.lambda)
  initCritOptDirection(crit, fun)
}

# sets the opt.direction to minimize or maximize depending on the object function.
initCritOptDirection = function(crit, fun) {
  if (crit$opt.direction == "objective") {
    crit$opt.direction = ifelse(shouldBeMinimized(fun), "minimize", "maximize")
  }
  crit
}
