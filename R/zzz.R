#' @import BBmisc
#' @import checkmate
#' @import lhs
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import plyr
#' @import reshape2
#' @importFrom soobench is_soo_function
#' @importFrom soobench global_minimum
NULL

.onAttach = function(libname, pkgname) {
  parallelRegisterLevels(package = "mlrMBO", levels = c("propose.points", "feval"))
}
