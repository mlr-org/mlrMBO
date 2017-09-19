#' @title Set MBO options.
#'
#' @description
#' Creates a control object for MBO optimization.
#'
#' @param n.objectives [\code{integer(1)}]\cr
#'   How many objectives are to be optimized? \code{n.objectives = 1} implies normal single
#'   criteria optimization, \code{n.objectives > 1} implies multicriteria optimization.
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
#'   Sometimes it is favourable to transform the target function values before modelling. Provide a
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
#'   If \code{schedule.method} is asyn then this is the path where all intermediate results will be stored.
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
#' @param schedule.method [\code{character(1)}]\cr
#'   Choose wich scheduling method for multipoint evaluation should be used. 
#'   Default is \dQuote{none} for no scheduling.
#'   If you want scheduling chose one of the following options but make sure to use it together with \code{parallelMap::parallelStart*(..., level = "mlrMBO.asyn")}:
#'   \dQuote{smartParallelMap}: Needs more \code{propose.points} then \code{schedule.nodes}. 
#'     Will propose more points then cores and only evaluate the most efficient ones.
#'     Runtimes for scheduling will be estimated.
#'   \dQuote{asyn}: Starts a different mbo routine, where the function is evaluated as soon as there are free resources. For this it is important to set \code{asyn.impute.method} as the proposal for a new point will take into account the ongoing evaluations which just have an NA y value. 
#' @param schedule.nodes [\code{integer(1)}]\cr
#'   If > 1 we try to schedule the proposed points in a smart way on the given numbers of threads.
#'   You need to set the value for \code{propose.points} higher then the number of available cores. 
#'   Not all proposed points will be evaluated.
#'   Default is \dQuote{1}.
#' @param schedule.priority [\code{character(1)}]\cr
#'    How should the sheduler priorotize points? The options are 
#'    \dQuote{infill}: Priorize points with promising infill criterion vlaue.
#'    \dQuote{explore}: Priorize points with high lambda part of the cb.
#'    \dQuote{exploit}: Priorize points with a low lambda part of the cb.
#'    \dQuote{balanced}: Priorize points with a lambda part of the cb which is close to the given \code{crit.cb.lambda} value in \code{setMBOControlInfill}.
#' @param schedule.priority.time [\code{logical(1)}]\cr
#'    Priorize by time? Meaning short predicted jobs will run first. Default is \code{FALSE}.
#' @param schedule.fill.random [\code{logical(1)}]\cr
#'    If scheduling does not fill all nodes, should we fill it with random points which have fast enough runtime? Default is \code{TRUE}.
#' @param schedule.cluster [\code{character{1}}]\cr
#'    How priority refinement via clustering is used for scheduling
#'    \dQuote{none}: No priority refinement via clustering is used.
#'    \dQuote{priority}: Refine job prioities based on their distance in the domain space, number of clusters < number of jobs
#'    \dQuote{distance}: Refine job priorities based on their distance in the domain space, number of clusters = number of jobs
#'    \dQuote{time}: Refine job priorities based on their distance in the domain space, number of clusters < number of jobs, shortest job per cluster gets highest priority.
#'   Default is \code{none}.
#' @param time.model.trafo.log [\code{logical(1)}]\cr
#'    Should the time model be learned on log-transformed times? Default is \code{FALSE}.
#' @param asyn.wait.for.proposals [\code{logical(1)}]\cr
#'    Should waiting be enabled for proposals? Default is \code{TRUE}.
#' @param asyn.skip.filtered.propsals [\code{logical(1)}]\cr
#'    If poposed points are filtered. 
#'    Should we then ignore those results and restart point proposal to find a point a bit further away.
#'    Only works with \code{filter.proposed.points = TRUE}. Default is \code{FALSE}.
#' @param asyn.cleanup [\code{logical(1)}]\cr
#'    Clean asyn files after run? Default is \code{FALSE}.
#' @param asyn.impute.method [\code{character(1)}]\cr
#'    Choices are:
#'      \dQuote{min} take minimum of y-values.
#'      \dQuote{max} take maximum of y-values.
#'      \dQuote{mean} take prediction at point x based on previous evaluations.
#'      \dQuote{noisymean} take prediction + rnorm(se) at point x based on previous evaluations.
#'      \dQuote{quantilemean} build multiple models with quantile imputed y values
#'   Default is \code{min}.
#' @param check.constant.model \code{logical(1)}\cr
#'    Should we check if the model just proposes constant values after each model build.
#'    (Only works for Focussearch for now)
#' @param asyn.impute.mc.iters \code{integer(1)}\cr
#'    Number of simulated kriging curves used to estimate the EEI for busy points.
#'    Default is 50.
#' @param asyn.impute.quantiles \code{numeric}\cr
#'    As an alternative to the monte carlo sampling above you can define quantiles to be drawn at busy points.
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
  save.file.path = file.path(getwd(), "mlr_run.RData"),
  store.model.at = NULL,
  resample.at = integer(0),
  resample.desc = makeResampleDesc("CV", iter = 10),
  resample.measures = list(mse),
  output.num.format = "%.3g",
  schedule.method = "none",
  schedule.nodes = 1L,
  schedule.priority = "infill",
  schedule.priority.time = FALSE,
  schedule.fill.random = TRUE,
  schedule.cluster = "none",
  time.learner = makeLearner("regr.km"),
  time.model.trafo.log = FALSE,
  asyn.wait.for.proposals = TRUE,
  asyn.skip.filtered.propsals = FALSE,
  asyn.cleanup = FALSE,
  asyn.impute.method = "min",
  asyn.impute.quantiles = c(0.25,0.5,0.75),
  asyn.impute.mc.iters = 50,
  check.constant.model = FALSE
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

  if (n.objectives > 1 && length(y.name) == 1 && y.name == "y")
    y.name = paste("y", 1:n.objectives, sep = "_")
  assertCharacter(y.name, len = n.objectives, any.missing = FALSE)

  # If debug-mode, turn of saving.
  if (getOption("mlrMBO.debug.mode", default = FALSE))
    save.on.disk.at = integer(0L)

  assertNumeric(save.on.disk.at.time, lower = 0, finite = FALSE, len = 1)
  if (!is.null(store.model.at)) assertIntegerish(store.model.at)
  assertPathForOutput(dirname(save.file.path), overwrite = TRUE)
  assertClass(resample.desc, "ResampleDesc")
  assertList(resample.measures, types = "Measure")

  assertString(output.num.format)
  assertChoice(schedule.method, choices = c("none", "smartParallelMap", "asyn", "scheduleKnapsack"))
  if (schedule.method == "asyn") {
    if (propose.points > 1L) warning("For schedule.method='asyn' you normally would just use propose.points = 1")
    schedule.nodes = asInteger(schedule.nodes)
  } else {
  schedule.nodes = asInteger(schedule.nodes, upper = propose.points)
  }
  
  assertChoice(schedule.priority, choices = c("infill", "explore", "exploit", "balanced"))
  assertFlag(schedule.priority.time)
  assertFlag(schedule.fill.random)
  assertFlag(time.model.trafo.log)
  assertFlag(asyn.wait.for.proposals)
  assertFlag(asyn.skip.filtered.propsals)
  assertFlag(asyn.cleanup)
  assertChoice(schedule.cluster, choices = c("priority", "distance", "time","none"))
  assertChoice(asyn.impute.method, choices = c("min", "max", "mean", "noisymean", "quantilemean", "mc"))
  assertNumeric(asyn.impute.quantiles, lower = 0, upper = 1, any.missing = FALSE, null.ok = TRUE)
  assertInt(asyn.impute.mc.iters, lower = 1, null.ok = TRUE)

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
    schedule.method = schedule.method,
    schedule.nodes = schedule.nodes,
    schedule.priority = schedule.priority,
    schedule.priority.time = schedule.priority.time,
    schedule.fill.random = schedule.fill.random,
    schedule.cluster = schedule.cluster,
    time.learner = time.learner,
    time.model.trafo.log = time.model.trafo.log,
    asyn.wait.for.proposals = asyn.wait.for.proposals,
    asyn.skip.filtered.propsals = asyn.skip.filtered.propsals,
    asyn.cleanup = asyn.cleanup,
    asyn.impute.method = asyn.impute.method,
    asyn.impute.quantiles = asyn.impute.quantiles,
    asyn.impute.mc.iters = asyn.impute.mc.iters,
    multifid = FALSE
  )

  # set defaults for infill methods and other stuff
  control = setMBOControlInfill(control)
  control = setMBOControlTermination(control, iters = 10L)
  if (n.objectives == 1L && propose.points > 1L)
    control = setMBOControlMultiPoint(control)
  if (n.objectives > 1L)
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
  catf("Objectives                    : %s", x$n.objectives)
  catf("Points proposed per iter      : %i", x$propose.points)
  if (!is.null(x$trafo.y.fun)) {
    catf("y transformed before modelling: %s", attr(x$trafo.y.fun, "name"))
  }
  catf("")
  if (x$n.objectives > 1L) {
    catf("Multicrit Method              : %s", x$multicrit.method)
    catf("Infill criterion              : %s", x$infill.crit)
    catf("Infill optimizer              : %s", x$infill.opt)
    catf("Infill optimizer restarts     : %i", x$infill.opt.restarts)
  } else {
    if (x$propose.points == 1) {
      catf("Infill criterion              : %s", x$infill.crit)
      catf("Infill optimizer              : %s", x$infill.opt)
      catf("Infill optimizer restarts     : %i", x$infill.opt.restarts)
    } else {
      catf("Multipoint method             : %s", x$multipoint.method)
    }
    catf("Final point by                : %s", x$final.method)
  }
}
