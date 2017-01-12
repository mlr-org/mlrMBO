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

.onLoad = function(libname, pkgname) {
  parallelRegisterLevels(package = "mlrMBO", levels = c("propose.points", "feval"))
}

# some shortcuts for infill criteria
ei = makeMBOInfillCriterionEI()
mr = makeMBOInfillCriterionMeanResponse()
se = makeMBOInfillCriterionStandardError()
cb1 = makeMBOInfillCriterionCB()
cb2 = makeMBOInfillCriterionCB(cb.lambda = 2)
cb3 = makeMBOInfillCriterionCB(cb.lambda = 3)
cb4 = makeMBOInfillCriterionCB(cb.lambda = 4)
cb5 = makeMBOInfillCriterionCB(cb.lambda = 5)
aei = makeMBOInfillCriterionAEI()
eqi = makeMBOInfillCriterionEQI()
dib1 = makeMBOInfillCriterionDIB()
dib2 = makeMBOInfillCriterionDIB(cb.lambda = 2)
dib3 = makeMBOInfillCriterionDIB(cb.lambda = 3)
dib4 = makeMBOInfillCriterionDIB(cb.lambda = 4)
dib5 = makeMBOInfillCriterionDIB(cb.lambda = 5)
