#' @import BBmisc
#' @import checkmate
#' @import ggplot2
#' @import lhs
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import smoof
#' @useDynLib mlrMBO c_sms_indicator c_eps_indicator
NULL

.onLoad = function(libname, pkgname) {
  parallelRegisterLevels(package = "mlrMBO", levels = c("propose.points", "feval"))
}
