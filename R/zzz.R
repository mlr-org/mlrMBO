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
EI = makeMBOInfillCriterionEI()
MR = makeMBOInfillCriterionMeanResponse()
SE = makeMBOInfillCriterionStandardError()
CB1 = makeMBOInfillCriterionCB()
CB2 = makeMBOInfillCriterionCB(cb.lambda = 2)
CB3 = makeMBOInfillCriterionCB(cb.lambda = 3)
CB4 = makeMBOInfillCriterionCB(cb.lambda = 4)
CB5 = makeMBOInfillCriterionCB(cb.lambda = 5)
AEI = makeMBOInfillCriterionAEI()
EQI = makeMBOInfillCriterionEQI()
DIB1 = makeMBOInfillCriterionDIB()
DIB2 = makeMBOInfillCriterionDIB(cb.lambda = 2)
DIB3 = makeMBOInfillCriterionDIB(cb.lambda = 3)
DIB4 = makeMBOInfillCriterionDIB(cb.lambda = 4)
DIB5 = makeMBOInfillCriterionDIB(cb.lambda = 5)
