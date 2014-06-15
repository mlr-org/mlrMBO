#FIXME: briefly describe what is in optpath

#' @title Single-Objective result object.
#'
#' @description
#'
#' \itemize{
#' @return [\code{list}]:
#'   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#'   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x},
#'     either from evals during optimization or from requested final evaluations,
#'     if those were greater than 0.}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#' }
#' @name MBOSingleObjResult
#' @rdname MBOSingleObjResult
NULL
