#' @title Continues an mbo run from a save-file.
#'
#' @description
#' Useful if your optimization is likely to crash,
#' so you can continue from a save point and will not lose too much information and runtime.
#'
#' @param opt.state [\code{OptState} | \code{character(1)}]\cr
#'   Either the \code{save.state} slot of an \code{MBOResult} object, or a file path of a saved MBO state.
#'   See \code{save.on.disk.at} argument of \code{\link{MBOControl}} object.
#' @return See \code{\link{mbo}}.
#' @export
mboContinue = function(opt.state) {
  if (testCharacter(opt.state, len = 1L)) {
    opt.state = loadOptState(opt.state)
  }
  if (!inherits(opt.state, "OptState")) {
    stop("opt.state must be either an OptState object, or a file path of a saved OptState.")
  }
  state = getOptStateState(opt.state)
  if (state %nin% c("init", "iter")) {
    warningf("Tuning ended with %s. No need to continue. Simply returning stored result.", state)
    return(getOptResultMboResult(getOptStateOptResult(opt.state)))
  }
  final.opt.state = mboTemplate(opt.state)
  mboFinalize2(final.opt.state)
}
