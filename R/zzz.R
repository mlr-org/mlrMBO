#' @import BBmisc
#' @import checkmate
#' @import grDevices
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import smoof
#' @import stats
#' @import utils
#' @import data.table
#' @importFrom lhs randomLHS
#' @useDynLib mlrMBO c_sms_indicator c_eps_indicator
NULL

.onLoad = function(libname, pkgname) { # nocov start
  backports::import(pkgname)
  parallelRegisterLevels(package = "mlrMBO", levels = c("propose.points", "feval"))
} # nocov end

# some shortcuts for infill criteria

#' @rdname MBOInfillCrit
#' @section Predefined standard infill criteria:
#' \describe{
#' \item{crit.ei}{Expected Improvement}
#' \item{crit.mr}{Mean response}
#' \item{crit.se}{Standard error}
#' \item{crit.cb}{Confidence bound with lambda automatically chosen, see \code{\link{infillcrits}}}
#' \item{crit.cb1}{Confidence bound with lambda=1}
#' \item{crit.cb2}{Confidence bound with lambda=2}
#' \item{crit.aei}{Augmented expected improvement}
#' \item{crit.eqi}{Expected quantile improvement}
#' \item{crit.dib1}{Direct indicator-based with lambda=1}
#' }
#'
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.ei = makeMBOInfillCritEI()
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.mr = makeMBOInfillCritMeanResponse()
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.se = makeMBOInfillCritStandardError()
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb = makeMBOInfillCritCB()
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb1 = makeMBOInfillCritCB(cb.lambda = 1)
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb2 = makeMBOInfillCritCB(cb.lambda = 2)
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.aei = makeMBOInfillCritAEI()
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.eqi = makeMBOInfillCritEQI()
#' @rdname MBOInfillCrit
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.dib1 = makeMBOInfillCritDIB(cb.lambda = 1)
