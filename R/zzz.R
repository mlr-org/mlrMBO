#' @import BBmisc
#' @import checkmate
#' @import ggplot2
#' @import grDevices
#' @import lhs
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import smoof
#' @import stats
#' @import utils
#' @useDynLib mlrMBO c_sms_indicator c_eps_indicator
NULL

.onLoad = function(libname, pkgname) {
  parallelRegisterLevels(package = "mlrMBO", levels = c("propose.points", "feval"))
}
