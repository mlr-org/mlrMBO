#' Creates a control object for MBO optimization.
#'
#' @param minimize [\code{logical}]\cr
#'   Should target functions be minimized? One value par target function.
#'   Default is \code{TRUE} for ever target function.
#' @param noisy [\code{logical(1)}]\cr
#'   Is the target function noisy?
#'   Default is \code{FALSE}.
#' @param number.of.targets [\code{integer(1)}]\cr
#'   How many target functions does the function have? Greater than one for
#'   multicriteria optimization, default ist 1.
#' @param init.design.points [\code{integer(1)}]\cr
#'   Number of points in inital design.
#'   Only used if no design is given in \code{mbo} function.
#'   Default is 20.
#' @param init.design.fun [\code{function}]\cr
#'   Function from package lhs for the sequential design.
#'   Possible are: \code{maximinLHS}, \code{randomLHS}, \code{geneticLHS},
#'   \code{improvedLHS}, \code{optAugmentLHS}, \code{optimumLHS}.
#'   Only used if no design is given in \code{mbo} function.
#'   Default is \code{maximinLHS}.
#' @param init.design.args [\code{list}]\cr
#'   List of further arguments passed to \code{init.design.fun}.
#'   Only used if no design is given in \code{mbo} function.
#'   Default is empty list.
#' @param iters [\code{integer(1)}]\cr
#'   Number of sequential optimization steps.
#'   Default is 10.
#' @param propose.points [\code{integer(1)}]\cr
#'   Number of proposed / really evaluated points each iteration.
#'   Default is 1.
#' @param feature.impute [\code{character(1)}]\cr
#'   Method used for imputing features, which can / will be necessary for dependent parameters.
#'   Possible values are:
#'   \dQuote{up}: Numeric vars are imputed with 2 * upper bound.
#'   \dQuote{median}: Imputes NAs with median and add logical is.na variable.
#' @param multiFid.control [\code{MBOmultiFidControl(1)}]\cr
#'   Necessary if \code{infill.crit = "multiFid"}.
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
#'   Default is \dQuote{y_i}, i = 1, ..., number.of.targets.
#' @param impute.y.fun [\code{function(x, y, opt.path), ...)}*]\cr
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
#' @param suppress.eval.errors [\code{logical(1)}]\cr
#'   Should reporting of error messages during target function evaluations be suppressed?
#'   Only used if \code{impute.errors} is \code{TRUE}.
#'   Default is \code{TRUE}.
#' @param save.on.disk.at [\code{integer}] \cr
#'   Sequential optimization iteration when the actual state should be saved
#'   on disk. Iteration 0 denotes the initial design. If the optimization
#'   stops with an crucial error, it can be restarted with this file via the
#'   function \code{\link{mboContinue}}.
#'   Default is \code{0:iters+1}.
#' @param save.file.path [\code{character(1)}] \cr
#'   If \code{save.on.disk.at} is used, this is the name of the file where the data
#'   will be saved.
#'   Default \dQuote{mbo_run.RData} in your current working directory.
#' @param store.model.at [\code{integer}]\cr
#'   Sequential optimization iterations when the model should be saved.
#'   Iteration 0 is the model fit for the initial design, iters + 1 is a final
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
#' @param on.learner.error [\code{character(1)}]\cr
#'   See [\code{\link[mlr]{configureMlr}}].
#'   Default is \dQuote{stop}.
#' @param show.learner.output [\code{logical(1)}]\cr
#'   See [\code{\link[mlr]{configureMlr}}].
#'   Default is \code{FALSE}.
#' @param output.num.format [\code{logical(1)}]\cr
#'   Format string for the precision of the numeric output of mbo.
#' @return [\code{\link{MBOControl}}].
#' @aliases MBOControl
#' @export
makeMBOControl = function(number.of.targets = 1L,
  minimize = rep(TRUE, number.of.targets), noisy = FALSE,
  init.design.points = 20L, init.design.fun = maximinLHS, init.design.args = list(),
  iters = 10L, propose.points = 1L,
  feature.impute = "up",
  multiFid.control = NULL,
  final.method = "best.true.y", final.evals = 0L,
  y.name = "y",
  impute.y.fun = NULL,
  suppress.eval.errors = TRUE,
  save.on.disk.at = 0:(iters+1),
  save.file.path = file.path(getwd(), "mlr_run.RData"),
  store.model.at = iters,
  resample.at = integer(0), resample.desc = makeResampleDesc("CV", iter = 10), resample.measures = list(mse),
  on.learner.error = "warn", show.learner.output = FALSE,
  output.num.format = "%.3g"
) {

  number.of.targets = asInt(number.of.targets, lower = 1L)
  assertLogical(minimize, len = number.of.targets, any.missing = FALSE)
  assertFlag(noisy)

  init.design.points = asInt(init.design.points)
  assertFunction(init.design.fun)
  assertList(init.design.args)

  iters = asInt(iters, lower = 0L)
  propose.points = asInt(propose.points, lower = 1L)

  assertChoice(feature.impute, choices = c("up", "median"))

  # if (infill.crit == "multiFid")
  #   assertClass(multiFid.control, "MBOMultiFidControl")

  if (!is.null(impute.y.fun))
    assertFunction(impute.y.fun, args = c("x", "y", "opt.path"))

  assertFlag(suppress.eval.errors)

  assertChoice(final.method, choices = c("last.proposed", "best.true.y", "best.predicted"))
  final.evals = asInt(final.evals, lower = 0L)

  if (number.of.targets > 1 && length(y.name) == 1 && y.name == "y")
    y.name = paste("y", 1:number.of.targets, sep = "_")
  assertCharacter(y.name, len = number.of.targets, any.missing = FALSE)

  save.on.disk.at = asInteger(save.on.disk.at, any.missing = FALSE, lower = 0, upper = iters + 1)
  assertPathForOutput(save.file.path)

  if ((iters + 1) %nin% save.on.disk.at)
    warningf("You turned off the final saving of the optimization result. Make sure to save it yourself!")
  # If debug-mode, turn of saving.
  if (getOption("mlrMBO.debug.mode", default = FALSE))
    save.on.disk.at = NULL

  store.model.at = asInteger(store.model.at, any.missing = FALSE, lower = 0, upper = iters + 1)
  resample.at = asInteger(resample.at, any.missing = FALSE, lower = 0L, upper = iters + 1)
  assertClass(resample.desc, "ResampleDesc")
  assertList(resample.measures, types = "Measure")

  assertChoice(on.learner.error, choices = c("warn", "quiet", "stop"))
  assertFlag(show.learner.output)
  assertString(output.num.format)

  control = makeS3Obj("MBOControl",
    minimize = minimize,
    noisy = noisy,
    number.of.targets = number.of.targets,
    init.design.points = init.design.points,
    init.design.fun = init.design.fun,
    init.design.args = init.design.args,
    iters = iters,
    propose.points = propose.points,
    feature.impute = feature.impute,
    final.method = final.method,
    final.evals = final.evals,
    y.name = y.name,
    impute.y.fun = impute.y.fun,
    suppress.eval.errors = suppress.eval.errors,
    save.on.disk.at = save.on.disk.at,
    save.file.path = save.file.path,
    store.model.at = store.model.at,
    resample.desc = resample.desc,
    resample.at = resample.at,
    resample.measures = resample.measures,
    on.learner.error = on.learner.error,
    show.learner.output = show.learner.output,
    output.num.format = output.num.format
  )

  # set defaults for infill methods and other stuff
  control = setMBOControlInfill(control)
  control = setMBOControlMultiPoint(control)
  control = setMBOControlMultiCrit(control)

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
  catf("Objective                   : %s",
    collapsef("%s = %s!", x$y.name, ifelse(x$minimize, "min", "max"), sep = "; "))
  catf("Function type               : %s",  ifelse(x$noisy, "noisy", "deterministic"))
  catf("Init. design                : %i points", x$init.design.points)
  catf("Iterations                  : %i", x$iters)
  catf("Points proposed per iter:   : %i", x$propose.points)
  if (x$propose.points == 1) {
  catf("Infill criterion            : %s", x$control$crit)
  catf("Infill optimizer            : %s", x$infill.opt)
  catf("Infill optimizer restarts   : %i", x$infill.opt.restarts)
  } else {
  catf("Multipoint method           : %s", x$control$multipoint.method)
  }
  catf("Final point by              : %s", x$final.method)
}
