#' @import backports
#' @import BBmisc
#' @import checkmate
#' @import grDevices
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import smoof
#' @import stats
#' @import utils
#' @importFrom lhs randomLHS
#' @useDynLib mlrMBO c_sms_indicator c_eps_indicator
NULL

.onLoad = function(libname, pkgname) { # nocov start
  parallelRegisterLevels(package = "mlrMBO", levels = c("propose.points", "feval"))
} # nocov end

# some shortcuts for infill criteria

#' @rdname MBOInfillCriterion
#' @section Predefined standard infill criteria:
#' \describe{
#' \item{crit.ei}{Expected Improvement}
#' \item{crit.mr}{Mean response}
#' \item{crit.se}{Standard error}
#' \item{crit.cb}{Confidence bound with lambda automatically chosen, see \code{\link{infillcrits}}}
#' \item{crit.cb1}{Confidence bound with lambda=1}
#' \item{crit.cb2}{Confidence bound with lambda=2}
#' \item{crit.aei}{Augmeted expected improvement}
#' \item{crit.eqi}{Expected quantile improvement}
#' \item{crit.dib1}{Directed indicator based search with lambda=1}
#' }
#'
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.ei = makeMBOInfillCriterionEI()
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.mr = makeMBOInfillCriterionMeanResponse()
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.se = makeMBOInfillCriterionStandardError()
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb = makeMBOInfillCriterionCB()
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb1 = makeMBOInfillCriterionCB(cb.lambda = 1)
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb2 = makeMBOInfillCriterionCB(cb.lambda = 2)
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.aei = makeMBOInfillCriterionAEI()
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.eqi = makeMBOInfillCriterionEQI()
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.dib1 = makeMBOInfillCriterionDIB(cb.lambda = 1)
