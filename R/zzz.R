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
#' @section Predefined standard Infill Criteria:
#' For commonly used settings of the Infill criteria:
#' \describe{
#' \item{crit.ei}{Expected Improvement}
#' \item{crit.mr}{Mean Response}
#' \item{crit.se}{Standard Error}
#' \item{crit.cb}{Confidence Bound with Lambda dependent on Parameter Space}
#' \item{crit.cb1}{Confidence Bound with Lambda=1}
#' \item{crit.cb2}{Confidence Bound with Lambda=2}
#' \item{crit.cb3}{Confidence Bound with Lambda=3}
#' \item{crit.cb4}{Confidence Bound with Lambda=4}
#' \item{crit.cb5}{Confidence Bound with Lambda=5}
#' \item{crit.aei}{Augmeted expected improvement}
#' \item{crit.eqi}{Expected quantile improvement}
#' \item{crit.dib1}{Directed Indicator Based Search with Lambda=1}
#' \item{crit.dib2}{Directed Indicator Based Search with Lambda=2}
#' \item{crit.dib3}{Directed Indicator Based Search with Lambda=3}
#' \item{crit.dib4}{Directed Indicator Based Search with Lambda=4}
#' \item{crit.dib5}{Directed Indicator Based Search with Lambda=5}
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
crit.cb3 = makeMBOInfillCriterionCB(cb.lambda = 3)
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb4 = makeMBOInfillCriterionCB(cb.lambda = 4)
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.cb5 = makeMBOInfillCriterionCB(cb.lambda = 5)
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
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.dib2 = makeMBOInfillCriterionDIB(cb.lambda = 2)
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.dib3 = makeMBOInfillCriterionDIB(cb.lambda = 3)
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.dib4 = makeMBOInfillCriterionDIB(cb.lambda = 4)
#' @rdname MBOInfillCriterion
#' @export
#' @usage NULL
#' @docType NULL
#' @format NULL
#' @keywords NULL
crit.dib5 = makeMBOInfillCriterionDIB(cb.lambda = 5)
