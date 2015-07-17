#' @title Continues an mbo run from a save-file.
#'
#' @description
#' Useful if your optimization didn't terminate but you want a results nonetheless.
#'
#' @param file [\code{character(1)}]\cr
#'   File path of saved MBO state.
#'   See \code{save.on.disk.at} argument of \code{\link{MBOControl}} object.
#' @return See \code{\link{mbo}}.
#' @export
mboFinalize = function(file) {
  assertCharacter(file, len = 1L)
  opt.state = loadOptState(file)
  state = setOptStateState(opt.state, "finalized")
  getOptStateMboResult(opt.state)
}
