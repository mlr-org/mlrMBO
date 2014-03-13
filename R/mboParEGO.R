#'  Optimizes a multicrit optimization problem  with sequential model based
#'  optimization using the parEGO algorithm
#'
#' @param fun [\code{function(x, ...)}]\cr
#'   Fitness function to minimize. The first argument has to be a list of values.
#'   The function has to return a single numerical value.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param design [\code{data.frame} | NULL]\cr
#'   Initial design as data frame.
#'   If the parameters have corresponding trafo functions,
#'   the design must not be transformed before it is passed!
#'   If \code{NULL}, one is constructed from the settings in \code{control}.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner to model \code{fun}.
#' @param control [\code{\link{MBOControl}}]\cr
#'   Control object for mbo.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{list}]:
#'   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#'   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x}, either from evals during optimization or from requested final evaluations, if those were greater than 0.}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#'   \item{multipoint.lcb.lambdas [\code{matrix(iters, proposed.points)}]}{Sampled lambda values for multipoint lcb method.}
#' @export
#' @aliases MBOResult
mboParEGO = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, ...) {
  #FIXME: more param checks
  checkStuff(fun, par.set, design, learner, control)
  loadPackages(control)
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # configure mlr in an appropriate way
  configureMlr(on.learner.error = control$on.learner.error,
    show.learner.output=control$show.learner.output)

  # FIXME: I am unsure wether we should do this, but otherwise RF sucks
  # if it is a good idea it is not not general enuff
  if (inherits(learner, "regr.randomForest")) {
    learner = setHyperPars(learner, fix.factors=TRUE)
  }

  # get parameter ids repeated length-times and appended number
  rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  y.name = control$y.name
  opt.path = makeOptPathDF(par.set, y.name, control$minimize)
  times = numeric(0)

  # FIXME: trafo attribute is bad, consider user generated designs
  # generate initial design if none provided
  if (is.null(design)) {
    design.x = generateDesign(control$init.design.points, par.set,
      control$init.design.fun, control$init.design.args, trafo=FALSE)
  } else {
    # sanity check: are paramter values and colnames of design consistent?
    if(!setequal(setdiff(colnames(design), y.name), rep.pids))
      stop("Column names of design 'design' must match names of parameters in 'par.set'!")
    design.x = dropNamed(design, y.name)
    # if no trafo attribute provided we act on the assumption that the design is not transformed
    if ("trafo" %nin% attributes(design.x)) {
      attr(design.x, "trafo") = FALSE
    }
  }
  # reorder
  design.x = design.x[, rep.pids, drop=FALSE]
  xs = dfRowsToList(design.x, par.set)
  # we now have design.x and its rows as lists in xs

  # compute y-values if missing or initial design generated above
  if (all(y.name %in% colnames(design.x))) {
    design.y = design[, y.name]
  } else if (!any(y.name %in% colnames(design.x))){
    if (show.info)
      messagef("Computing y column for design. Was not provided")
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, ... )
    design.y = evals$ys
    times = c(times, evals$times)
  } else {
    stop("Only part of y-values are provided. Don't know what to do - provide either all or none.")
  }
  design = cbind(design.x, design.y)
  # we now have design.y and design

  # add initial values to optimization path
  ys = convertRowsToList(design.y)
  Map(function(x,y) addOptPathEl(opt.path, x=x, y=y, dob=0), xs, ys)

  # do the mbo magic
  for (loop in seq_len(control$iters)) {
    # create a couple of scalar tasks to optimize and eval in parallel
    scalarTasks = makeScalarTasks(design, par.set, y.name, control = control)
    models = lapply(scalarTasks, train, learner = learner)
    # propose new points and evaluate target function
    prop.designs = lapply(models, proposePoints, par.set = par.set,
      control = control, opt.path = opt.path)
    prop.designs = do.call(rbind, prop.designs)
    xs = dfRowsToList(prop.designs, par.set)
    xs = lapply(xs, repairPoint, par.set = par.set)
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, ...)
    ys = convertRowsToList(evals$ys)
    # store in opt path
    mapply(addOptPathEl, xs, ys, MoreArgs = list(op = opt.path, dob = loop))
  }

  front.index = getOptPathParetoFront(opt.path, index = TRUE)
  pareto.front = getOptPathParetoFront(opt.path, index = FALSE)

  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])

  makeS3Obj("ParEGOResult",
    pareto.front = pareto.front,
    opt.path = opt.path
  )
}

# Print ParEGO result object.
#
# @param x [\code{\link{ParEGOResult}}]\cr
#   mbo result object instance.
# @param ... [any]\cr
#   Not used.
#' @S3method print ParEGOResult
print.ParEGOResult = function(x, ...) {
  op = x$opt.path
  print(tail(as.data.frame(x$op), 10))
}
