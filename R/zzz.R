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
crit.ei = makeMBOInfillCriterionEI()
crit.mr = makeMBOInfillCriterionMeanResponse()
crit.se = makeMBOInfillCriterionStandardError()
crit.cb1 = makeMBOInfillCriterionCB()
crit.cb2 = makeMBOInfillCriterionCB(cb.lambda = 2)
crit.cb3 = makeMBOInfillCriterionCB(cb.lambda = 3)
crit.cb4 = makeMBOInfillCriterionCB(cb.lambda = 4)
crit.cb5 = makeMBOInfillCriterionCB(cb.lambda = 5)
crit.aei = makeMBOInfillCriterionAEI()
crit.eqi = makeMBOInfillCriterionEQI()
crit.dib1 = makeMBOInfillCriterionDIB()
crit.dib2 = makeMBOInfillCriterionDIB(cb.lambda = 2)
crit.dib3 = makeMBOInfillCriterionDIB(cb.lambda = 3)
crit.dib4 = makeMBOInfillCriterionDIB(cb.lambda = 4)
crit.dib5 = makeMBOInfillCriterionDIB(cb.lambda = 5)
