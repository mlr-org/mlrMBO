#' @title Continues an mbo run from a save-file.
#'
#' @description
#' Useful if your optimization is likely to crash,
#' so you can continue from a save point and will not lose too much information and runtime.
#'
#' @param file [\code{character(1)}]\cr
#'   File path of saved MBO state.
#'   See \code{save.on.disk.at} argument of \code{\link{MBOControl}} object.
#' @return See \code{\link{mbo}}.
#' @export
mboContinue = function(file) {
  assertCharacter(file, len = 1L)
  tuningState = loadTuningState(file)
  mboTemplate(tuningState)
}
