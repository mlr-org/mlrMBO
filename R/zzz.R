#' @import BBmisc
#' @import checkmate
#' @import ggplot2
#' @import RColorBrewer
#' @import gridExtra
#' @import lhs
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import plyr
#' @import reshape2
#' @useDynLib mlrMBO c_sms_indicator c_eps_indicator
NULL

.onAttach = function(libname, pkgname) {
  parallelRegisterLevels(package = "mlrMBO", levels = c("propose.points", "feval"))
}
