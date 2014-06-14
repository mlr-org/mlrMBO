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
  checkArg(file, "character", len = 1)
  if (!file.exists(file)) {
    stopf("Specified file does not exist.")
  }

  # load the file and check if every required object exists
  f = load2(file, simplify = FALSE)
  print(names(f))
  if (any(c("fun", "learner", "par.set", "opt.path", "control", "show.info", "more.args") %nin% names(f)))
    stopf("Whatever the file contained you specified - it was not saved by mbo. Please specify a correct file.")

  # Restart mbo
  if (f$control$number.of.targets == 1L) {
    mboSingleObj(f$fun, f$par.set, f$opt.path, f$learner, f$control, f$show.info, f$more.args, continue = f$opt.path)
  } else {
    if (f$control$multicrit.method == "parego")
      mboParego(f$fun, f$par.set, f$opt.path, f$learner, f$control, f$show.info, f$more.args, continue = f$opt.path)
  }
}
