#' @title Initialize an MBO infill criterion.
#'
#' @description 
#'   Some infill criteria have parameters that are dependent on values in the parameter set, design,
#'   used learner or other control settings.
#'   To actually set these default values, this function is called, which returns a fully 
#'   initialized [\code{\link{MBOInfillCriterion}}].
#'   This function is mainly for internal use. If a custom infill criterion is created, it may be 
#'   required to create a seperate method \code{initCrit.InfillCRitID} where \code{ID} is the 
#'   \code{id} of the custom \link{MBOInfillCriterion}.
#'
#' @param crit [\code{\link{MBOInfillCriterion}}]\cr
#'   Uninitialized infill criterion.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param design
#'   Sampling plan.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner from mlr, which is used as a surrogate to model our fitness function.
#' @param control [\code{\link{MBOControl}}]\cr
#'   MBO control object.
#' @return [\code{\link{MBOInfillCriterion}}]
#' @export
initCrit = function(crit, par.set, design, learner, control) {
  UseMethod("initCrit")
}

#' @export
initCrit.default = function(crit, par.set, design, learner, control) {
  return(crit)
}

#' @export
initCrit.InfillCritCB = function(crit, par.set, design, learner, control) {
  cb.lambda = crit$cb.lambda
  if (is.null(cb.lambda))
    cb.lambda = ifelse(isSimpleNumeric(par.set), 1, 2)
  makeMBOInfillCriterionCB(cb.lambda)
}