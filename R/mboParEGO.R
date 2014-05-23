#'  Optimizes a multicrit optimization problem  with sequential model based
#'  optimization using the parEGO algorithm
#'
#' @param fun [\code{function(x, ...)}]\cr
#'   Fitness functions to minimize. The first argument has to be a list of values.
#'   The function has to return a numerical vector of the length defined via the
#'   parameter number.of.targets in \code{control}.
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
#' @param more.args [any]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{list}]:
#'   \item{pareto.front [\code{matrix}]}{Pareto Front of all evaluated points.}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#' @export
mboParEGO = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, more.args=list()) {
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

  # generate initial design
  mboDesign = generateMBODesign(design, fun, par.set, control, show.info, oldopts, more.args)
  design = cbind(mboDesign$design.x, mboDesign$design.y)
  opt.path = mboDesign$opt.path
  times = mboDesign$times
  y.name = control$y.name
  # we now have design.y and design

  # do the mbo magic
  for (loop in seq_len(control$iters)) {
    # create a couple of scalar tasks to optimize and eval in parallel
    scalarTasks = makeScalarTasks(design, par.set, y.name, control = control)
    models = lapply(scalarTasks, train, learner = learner)
    # propose new points and evaluate target function
    prop.designs = lapply(models, proposePoints, par.set = par.set,
      control = control, opt.path = opt.path)
    prop.designs = do.call(rbind, lapply(prop.designs, function(x) x$prop.points))
    xs = dfRowsToList(prop.designs, par.set)
    xs = lapply(xs, repairPoint, par.set = par.set)
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, more.args)
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
  print(x$pareto.front)
  print(tail(as.data.frame(x$op), 10))
}
