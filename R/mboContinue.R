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
  opt.state = loadOptState(file)
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  state = getOptStateState(opt.state)
  if (control$schedule.method == "asyn") {
    if (is.finite(control$time.budget) && !is.finite(control$exec.time.budget)) {
      stopf("Continuation of asyn with given time.budget does not work yet.")
    } else if (is.finite(control$time.budget) && is.finite(control$exec.time.budget)) {
      warningf("Continuation of asyn disables the time.budget (%i) and we only use exec.time.budget (%i)", control$time.budget, control$exec.time.budget)
      control$time.budget = Inf #FIXME for obvious reasons
    }
    cleanProposals(getOptStateOptProblem(opt.state))
    final.opt.state = mboAsynTemplate(opt.state)
  } else if (control$infill.crit == "random") {
    stopf("RandomSearch ist not supported to be continued. Return last state", state)
    return(getOptResultMboResult(getOptStateOptResult(opt.state)))
  } else if (state %nin% c("init", "iter")) {
    warningf("Tuning ended with %s. No need to continue. Simply returning stored result.", state)
    return(getOptResultMboResult(getOptStateOptResult(opt.state)))
  } else {
    final.opt.state = mboTemplate(opt.state)  
  }
  mboFinalize2(final.opt.state)
}
