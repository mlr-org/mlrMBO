#' @title Perform an mbo run on a test function and and visualize what happens.
#'
#' @description
#' Usually used for 1D or 2D examples,
#' useful for figuring out how stuff works and for teaching purposes.
#' Currently only parameter spaces with numerical parameters are supported.
#' For visualization, run \code{plotExampleRun} on the resulting object.
#' What is displayed is documented here: \code{\link{plotExampleRun}}.
#' Rendering the plots without displaying them is possible via the function
#' \code{\link{renderExampleRunPlot}}.
#'
#' Please note the following things:
#' - The true objective function (and later everything which is predicted from our surrogate model)
#'   is evaluated on a regular spaced grid. These evaluations are stored in the result object.
#'   You can control the resolution of this grid via \code{points.per.dim}.
#'   Parallelization of these evaluations is possible with the R package parallelMap on the level \code{mlrMBO.feval}.
#' - In every iteration the fitted, approximating surrogate model is stored in the result object
#'   (via \code{store.model.at} in \code{control}) so we can later visualize it quickly.
#' - The global optimum of the function (if defined) is extracted from the passed smoof function.
#' - If the passed objective function \code{fun} does not provide the true, unnoisy objective function
#'   some features will not be displayed (for example the gap between the best point so far and the global optimum).
#'
#' @inheritParams mbo
#' @param points.per.dim [\code{integer}]\cr
#'   Number of (regular spaced) locations at which to
#'   sample the \code{fun} function per dimension.
#'   Default is 50.
#' @param noisy.evals [\code{integer(1)}]\cr
#'   Number of function evaluations per point if \code{fun} is noisy.
#'   Default is 10.
#' @return [\code{MBOExampleRun}]
#' @export
exampleRun = function(fun, design = NULL, learner = NULL, control,
  points.per.dim = 50, noisy.evals = 10, show.info = getOption("mlrMBO.show.info", TRUE)) {

  assertClass(fun, "smoof_single_objective_function")
  par.set = getParamSet(fun)
  par.types = getParamTypes(par.set)
  n.params = sum(getParamLengths(par.set))
  noisy = isNoisy(fun)
  control$noisy = noisy
  control$minimize = shouldBeMinimized(fun)
  learner = checkLearner(learner, control, fun)
  assertClass(control, "MBOControl")
  points.per.dim = asCount(points.per.dim, positive = TRUE)
  noisy.evals = asCount(noisy.evals, positive = TRUE)
  fun.mean = smoof::getMeanFunction(fun)
  assertFlag(show.info)

  if (smoof::hasGlobalOptimum(fun))
    global.opt = smoof::getGlobalOptimum(fun)$value
  else
    global.opt = NA_real_

  if (control$n.objectives != 1L)
    stopf("exampleRun can only be applied for single-objective functions, but you have %i objectives! Use 'exampleRunMultiObj'!",
      control$n.objectives)
  if (n.params >= 3L)
    stopf("exampleRun can only be applied for functions with at most 2 dimensions, but you have %iD", n.params)

  control$store.model.at = seq_len(control$iters+1)
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  name.y = control$y.name

  if (show.info) {
    messagef("Evaluating true objective function at %s points.",
      if (!noisy) {
        if (n.params == 1) {
          sprintf("%i", points.per.dim)
        } else {
          sprintf("%i x %i", points.per.dim, points.per.dim)
        }
      }
    )
  }

  # if noisy and we have the mean function, use it
  if (is.null(fun.mean)) {
    evals = evaluate(fun, par.set, n.params, par.types, noisy, noisy.evals, points.per.dim, names.x, name.y, seq_along(control$multifid.lvls))
  } else {
    evals = evaluate(fun.mean, par.set, n.params, par.types, noisy = FALSE, noisy.evals = 1, points.per.dim, names.x, name.y, multifid.lvls = seq_along(control$multifid.lvls))
  }

  if (is.na(global.opt))
    global.opt.estim = ifelse(shouldBeMinimized(fun), min(evals[, name.y]), max(evals[, name.y]))
  else
    global.opt.estim = NA_real_

  #show some info on console
  if (show.info) {
    messagef("Performing MBO on function.")
    if (is.null(design))
      messagef("Initial design: %i. Sequential iterations: %i.", control$init.design.points, control$iters)
    messagef("Learner: %s. Settings:\n%s", learner$id, getHyperParsString2(learner, show.missing.values = FALSE))
  }

  # run optimizer now
  res = mbo(fun, design = design, learner = learner, control = control, show.info = show.info)

  # compute true y-values if deterministic function is known
  y.true = NA
  if (!is.null(fun.mean)) {
    y.true = vnapply(convertRowsToList(getOptPathX(res$opt.path), name.list = TRUE, name.vector = TRUE), fun.mean)
  }

  if (control$multifid) {
    n.params = n.params - 1
    par.set = dropParams(par.set, ".multifid.lvl")
    par.types = getParamTypes(par.set)
    names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  }

  makeS3Obj("MBOExampleRun",
    fun.mean = fun.mean,
    par.set = par.set,
    y.true = y.true,
    n.params = n.params,
    par.types = par.types,
    names.x = names.x,
    name.y = name.y,
    points.per.dim = points.per.dim,
    evals = evals,
    global.opt = global.opt,
    global.opt.estim = global.opt.estim,
    learner = learner,
    control = res$control,
    mbo.res = res
  )
}

#' @export
print.MBOExampleRun = function(x, ...) {
  gap = calculateGap(as.data.frame(x$mbo.res$opt.path), x$global.opt, x$control)
  catf("MBOExampleRun")
  catf("Number of parameters        : %i", x$n.params)
  catf("Parameter names             : %s", collapse(x$names.x))
  catf("Parameter types             : %s", collapse(x$par.types))
  catf("%-28s: %.4e", getGlobalOptString(x), getGlobalOpt(x))
  catf("Gap for best point          : %.4e", gap)
  catf("True points per dim.        : %s", collapse(x$points.per.dim))
  print(x$control)
  catf("Learner                     : %s", x$learner$id)
  catf("Learner settings:\n%s", getHyperParsString2(x$learner, show.missing.values = FALSE))
  mr = x$mbo.res
  op = mr$opt.path
  catf("Recommended parameters:")
  catf(paramValueToString(op$par.set, mr$x))
  catf("Objective: %s = %.3e\n", op$y.names[1], mr$y)
}

getEvals = function(fun, par.set, noisy, noisy.evals, points.per.dim, names.x, name.y) {
  xs.trafo = generateGridDesign(par.set, points.per.dim, trafo = TRUE)
  xs.trafo.list = convertRowsToList(xs.trafo, name.list = TRUE, name.vector = TRUE)
  ys = parallelMap(function(x) {
    if (noisy) {
      mean(replicate(noisy.evals, fun(x)))
    } else {
      fun(x)
    }
  }, xs.trafo.list, level = "mlrMBO.feval", simplify = TRUE)
  evals = cbind(xs.trafo, y = ys)
  colnames(evals) = c(names.x, name.y)
  return(evals)
}

evaluate = function(fun, par.set, n.params, par.types, noisy, noisy.evals, points.per.dim, names.x, name.y, multifid.lvls = numeric(0)) {
  if (!noisy && n.params == 1L && par.types == "discrete")
    stopf("ExampleRun does not make sense with a single deterministic discrete parameter.")
  if (length(multifid.lvls) && n.params %in% c(2L,3L) && all(par.types %in% c("numeric", "numericvector", "discrete", "integer")))
    return(getEvals(fun, par.set, noisy, noisy.evals, points.per.dim * length(multifid.lvls), names.x, name.y))
  if (n.params %in% c(1L, 2L) && all(par.types %in% c("numeric", "numericvector", "discrete"))) {
    return(getEvals(fun, par.set, noisy, noisy.evals, points.per.dim, names.x, name.y))
  }
}

#' Helper function which returns the (estimated) global optimum.
#'
#' @param run [\code{MBOExampleRun}]\cr
#'   Object of type \code{MBOExampleRun}.
#' @return [\code{numeric(1)}]. (Estimated) global optimum.
#' @export
getGlobalOpt = function(run) {
  ifelse(is.na(run$global.opt), run$global.opt.estim, run$global.opt)
}

getGlobalOptString = function(run) {
  sprintf("Global Opt (%s)",  ifelse(is.na(run$global.opt), "estimated", "known"))
}
