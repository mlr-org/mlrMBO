#' @title Set MBO options.
#'
#' @description
#' Creates a control object for MBO optimization.
#'
#' @param n.objectives [\code{integer(1)}]\cr
#'   How many objectives are to be optimized? \code{n.objectives = 1} implies normal single
#'   criteria optimization, \code{n.objectives > 1} implies multi-objective optimization.
#'   Default is 1.
#' @param propose.points [\code{integer(1)}]\cr
#'   Number of proposed / really evaluated points each iteration.
#'   Default is 1.
#' @param final.method [\code{character(1)}]\cr
#'   How should the final point be proposed. Possible values are:
#'   \dQuote{best.true.y}: Return best point ever visited according to true value of target function.
#'   Can be bad if target function is noisy.
#'   \dQuote{last.proposed}: Return the last point proposed by the model.
#'   \dQuote{best.predicted}: Use the final model to predict all points ever visited and use the best one.
#'   This might average-out noisy function values.
#'   Default is: \dQuote{best.true.y}.
#' @param final.evals [\code{integer(1)}]\cr
#'   How many target function evals should be done at final point to reduce noise?
#'   Default is 0.
#' @param y.name [\code{character}]\cr
#'   Vector for names of y-columns for target values in optimization path.
#'   Default is \dQuote{y_i}, i = 1, ..., n.objectives.
#' @param impute.y.fun [\code{function(x, y, opt.path, ...)}*]\cr
#'   Functions that gets triggered if your objective evaluation produced
#'   a) an exception b) a return object of invalid type c) a numeric vector that
#'   contains \code{NA}, \code{NaN}, \code{Inf}.
#'   You now have a chance to handle this. You are expected to return a numeric vector
#'   of the correct length with concrete values.
#'   The optimization path will show some information whether y-values where imputed
#'   and what the original, faulty object was.
#'   \code{x} is the current x-value, \code{y} the current (invalid) y-object (or an error object)
#'   and \code{opt.path} the current optimization path.
#'   Default is \code{NULL} which means to stop if the objective function did not produce the desired
#'   result.
#' @param trafo.y.fun [\code{MBOTrafoFunction}]\cr
#'   Sometimes it is favorable to transform the target function values before modeling. Provide a
#'   MBO transformation function to do so.
#' @param suppress.eval.errors [\code{logical(1)}]\cr
#'   Should reporting of error messages during target function evaluations be suppressed?
#'   Only used if \code{impute.errors} is \code{TRUE}.
#'   Default is \code{TRUE}.
#' @param save.on.disk.at [\code{integer}] \cr
#'   Sequential optimization iteration when the actual state should be saved
#'   on disk. Iteration 0 denotes the initial design. If the optimization
#'   stops with an crucial error, it can be restarted with this file via the
#'   function \code{\link{mboContinue}}.
#'   Default is \code{integer(0L)}, i. e., not to save.
#' @param save.on.disk.at.time [\code{integer}] \cr
#'   Same as above. But here you define the time which have to be passed until the last save in seconds. Any finite value will lead to save at end.
#'   Default is \code{Inf}, i. e., not to save ever.
#' @param save.file.path [\code{character(1)}] \cr
#'   If \code{save.on.disk.at} is used, this is the name of the file where the data
#'   will be saved.
#'   Default \dQuote{mbo_run.RData} in your current working directory.
#' @param store.model.at [\code{integer}]\cr
#'   Sequential optimization iterations when the model should be saved.
#'   Iteration 1 is the model fit for the initial design, iters + 1 is a final
#'   save containing the final results of the optimization. .
#'   Default is \code{iters + 1}.
#' @param resample.at [\code{integer}]\cr
#'   At which iterations should the model be resampled and assessed?
#'   Iteration 0 does some resampling on the initial design.
#'   Default is none.
#' @param resample.desc [\code{\link[mlr]{ResampleDesc}}]\cr
#'   How should the model be resampled?
#'   Default is 10-fold CV.
#' @param resample.measures [list of \code{\link[mlr]{Measure}}]\cr
#'   Performance measures to assess model with during resampling.
#'   Default is \code{\link[mlr]{mse}}.
#' @param output.num.format [\code{logical(1)}]\cr
#'   Format string for the precision of the numeric output of mbo.
#' @param on.surrogate.error [\code{character(1)}]\cr
#'   What should happen when the surrogate learner can not train the model.
#'   Possible values are:
#'   \dQuote{stop}: R exception is generated.
#'   \dQuote{warn}: The error will be converted to a waring and a random point will be proposed.
#'   \dQuote{quiet}: Same as “warn” but without the warning.
#'   This will overwrite the mlr setting \code{on.learner.error} for the surrogate learner.
#'   Default is: \dQuote{stop}.
#'
#' @return [\code{\link{MBOControl}}].
#' @aliases MBOControl
#' @family MBOControl
#' @export
makeMBOControl = function(n.objectives = 1L,
  propose.points = 1L,
  final.method = "best.true.y", final.evals = 0L,
  y.name = "y",
  impute.y.fun = NULL,
  trafo.y.fun = NULL,
  suppress.eval.errors = TRUE,
  save.on.disk.at = integer(0L),
  save.on.disk.at.time = Inf,
  save.file.path = file.path(getwd(), "mlrMBO_run.RData"),
  store.model.at = NULL,
  resample.at = integer(0),
  resample.desc = makeResampleDesc("CV", iter = 10),
  resample.measures = list(mse),
  output.num.format = "%.3g",
  on.surrogate.error = "stop"
) {

  n.objectives = asInt(n.objectives, lower = 1L)

  propose.points = asInt(propose.points, lower = 1L)

  if (!is.null(impute.y.fun))
    assertFunction(impute.y.fun, args = c("x", "y", "opt.path"))

  if (!is.null(trafo.y.fun))
    assertClass(trafo.y.fun, "MBOTrafoFunction")

  assertFlag(suppress.eval.errors)

  assertChoice(final.method, choices = c("last.proposed", "best.true.y", "best.predicted"))
  final.evals = asInt(final.evals, lower = 0L)

  if (n.objectives > 1L && length(y.name) == 1L && y.name == "y")
    y.name = paste("y", seq_len(n.objectives), sep = "_")
  assertCharacter(y.name, len = n.objectives, any.missing = FALSE)

  # If debug-mode, turn of saving.
  if (getOption("mlrMBO.debug.mode", default = FALSE))
    save.on.disk.at = NULL

  assertNumeric(save.on.disk.at.time, lower = 0, finite = FALSE, len = 1)
  assertClass(resample.desc, "ResampleDesc")
  assertList(resample.measures, types = "Measure")

  assertString(output.num.format)

  assertChoice(on.surrogate.error, c("warn", "stop", "quiet"))

  control = makeS3Obj("MBOControl",
    n.objectives = n.objectives,
    propose.points = propose.points,
    final.method = final.method,
    final.evals = final.evals,
    y.name = y.name,
    impute.y.fun = impute.y.fun,
    trafo.y.fun = trafo.y.fun,
    suppress.eval.errors = suppress.eval.errors,
    save.on.disk.at = save.on.disk.at,
    save.on.disk.at.time = save.on.disk.at.time,
    save.file.path = save.file.path,
    store.model.at = store.model.at,
    resample.desc = resample.desc,
    resample.at = resample.at,
    resample.measures = resample.measures,
    output.num.format = output.num.format,
    on.surrogate.error = on.surrogate.error
  )

  # set defaults for infill methods and other stuff
  control = setMBOControlInfill(control)
  control = setMBOControlTermination(control, iters = 10L)
  if (n.objectives == 1L && propose.points > 1L)
    control = setMBOControlMultiPoint(control)
  if (n.objectives > 1L)
    control = setMBOControlMultiObj(control)
  return(control)
}

#' Print mbo control object.
#'
#' @param x [\code{\link{MBOControl}}]\cr
#'   Control object.
#' @param ... [any]\cr
#'   Not used.
#' @export
print.MBOControl = function(x, ...) {
  catf("Objectives                    : %s", x$n.objectives)
  catf("Points proposed per iter      : %i", x$propose.points)
  if (!is.null(x$trafo.y.fun)) {
    catf("y transformed before modelling: %s", attr(x$trafo.y.fun, "name"))
  }
  catf("")
  if (x$n.objectives > 1L) {
    catf("Multi-objective Method        : %s", x$multiobj.method)
    print(x$infill.crit)
    catf("Infill optimizer              : %s", x$infill.opt)
    catf("Infill optimizer restarts     : %i", x$infill.opt.restarts)
  } else {
    if (x$propose.points == 1) {
      print(x$infill.crit)
      catf("Infill optimizer              : %s", x$infill.opt)
      catf("Infill optimizer restarts     : %i", x$infill.opt.restarts)
    } else {
      catf("Multi-point method            : %s", x$multipoint.method)
    }
    catf("Final point by                : %s", x$final.method)
  }
}
