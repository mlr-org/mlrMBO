#' @title Continues an mbo run from a save-file.
#'
#' @description
#' Useful if your optimization didn't terminate but you want a results nonetheless.
#'
#' @param file [\code{character(1)}]\cr
#'   File path of saved MBO state.
#'   See \code{save.on.disk.at} argument of \code{\link{MBOControl}} object.
#' @param save [\code{logical(1)}]\cr
#'   Save the finalized state on hard disk? This will make continuation difficult.
#'   Default is \code{FALSE}
#' @return See \code{\link{mbo}}.
#' @export
mboFinalize = function(file, save = FALSE) {
  assertCharacter(file, len = 1L)
  opt.state = loadOptState(file)
  state = getOptStateState(opt.state)
  if (state %in% getTerminateChars()) {
  warningf("Optimization ended with %s. No need to continue. Simply returning stored result.", state)
  return(getOptResultMboResult(getOptStateOptResult(opt.state)))
  }
  state = setOptStateState(opt.state, getTerminateChars("manual"))
  mbo.res = makeOptStateMboResult(opt.state)
  if (save) {
    saveOptState(opt.state)
  }
  mbo.res
}
