#'  Restarts a mbo-run from a save-file. Useful if your optimization is likely to
#'  crash, so you can start again from the iteration the crash happend and to not lose
#'  to much information.
#'
#' @param file [\code{character(1)}]\cr
#'   Saved MBO run. Can be obtained by setting the \code{save.on.disk.at} argument
#'   of the mbo control object.
#' @export
restartSavedMBO = function(file) {
  
  checkArg(file, "character", len = 1)
  if (!file.exists(file)) {
    stopf("Specified file does not exist.")
  }
  
  # load the file and check if every required object exists
  load(file)
  if (!all(c("fun", "par.set", "opt.path", "learner", "control", "show.info", "more.args") %in% ls()))
    stopf("Whatever the file contained you specified - it was not saved by mbo. Please specify a correct file.")
  
  # Restart mbo
  mbo(fun = fun, par.set = par.set, design = opt.path,
    learner = learner, control = control, show.info = show.info, more.args = more.args)
}