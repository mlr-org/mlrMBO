#' @title OptPath in mlrMBO
#'
#' @description
#' In mlrMBO the \code{\link[ParamHelpers]{OptPath}} contains extra information next to the information documented in \code{\link[ParamHelpers]{OptPath}}.
#'
#' The extras are:
#' \describe{
#' \item{train.time}{Time to train the model(s) that produced the points. Only the first slot of the vector is used (if we have multiple points), rest are NA.}
#' \item{propose.time}{Time needed to propose the point. If we have individual timings from the proposal mechanism, we have one different value per point here. If all were generated in one go, we only have one timing, we store it in the slot for the first point, rest are NA.}
#' \item{errors.model}{Possible Error Messages. If point-producing model(s) crashed they are replicated for all n points, if only one error message was passed we store it for the first point, rest are NA.}
#' \item{prop.type}{Type of point proposal. Possible values are
#'   \describe{
#'   \item{initdesign}{Points actually not proposed, but in the initial design.}
#'   \item{infill_x}{Here x is a placeholder for the selected infill criterion, e.g., infill_ei for expected improvement.}
#'   \item{random_interleave}{Uniformly sampled points added additionally to the proposed points.}
#'   \item{random_filtered}{If filtering of proposed points located too close to each other is active, these are replaced by random points.}
#'   \item{final_eval}{If \code{final.evals} is set in \code{\link{makeMBOControl}}: Final evaluations of the proposed solution to reduce noise in y.}
#'   }
#' }
#' \item{parego.weight}{Weight vector sampled for multi-point ParEGO}
#' \item{...}{Depending on the chosen infill criterion there will be additional columns, e.g. \code{se} and \code{mean} for the Expected Improvement)}
#' }
#'
#' Moreover, the user may pass additional \dQuote{user extras} by appending a named list
#' of scalar values to the return value of the objective function.
#'
#' @name mbo_OptPath
NULL
