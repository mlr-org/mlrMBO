#  Optimizes a multicrit optimization problem  with sequential model based
#  optimization using the parEGO algorithm
#
# @param fun [\code{function(x, ...)}]\cr
#   Fitness functions to minimize. The first argument has to be a list of values.
#   The function has to return a numerical vector of the length defined via the
#   parameter number.of.targets in \code{control}.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param design [\code{data.frame} | NULL]\cr
#   One of this 3:
#   - Initial design as data frame.
#     If the parameters have corresponding trafo functions,
#     the design must not be transformed before it is passed!
#   - A opt.path object:
#     The design and all saved infos will be extracted from this
#   - \code{NULL}:
#     The design is constructed from the settings in \code{control}.
# @param learner [\code{\link[mlr]{Learner}}]\cr
#   Regression learner to model \code{fun}.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object for mbo.
# @param show.info [\code{logical(1)}]\cr
#   Verbose output on console?
#   Default is \code{TRUE}.
# @param more.args [any]\cr
#   Further arguments passed to fitness function.
# @return [\code{list}]:
#   \item{pareto.front [\code{matrix}]}{Pareto Front of all evaluated points.}
#   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
mboParEGO = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, more.args=list()) {
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )
  
  # Calculate all possible weight vectors and save them
  Lambdas = combWithSum(control$parEGO.s, control$number.of.targets) / control$parEGO.s
  
  # generate initial design
  mboDesign = generateMBODesign(design, fun, par.set, control, show.info, oldopts, more.args)
  design = cbind(mboDesign$design.x, mboDesign$design.y)
  opt.path = mboDesign$opt.path
  times = mboDesign$times
  y.name = control$y.name
  # we now have design.y and design
  
  # initialize data.frame to save the used weights
  # FIXME: We want to save this information in the opt.path!
  weight.path = data.frame()
  
  # do the mbo magic
  start.loop = max(getOptPathDOB(opt.path)) + 1
  for (loop in start.loop:control$iters) {
    # create a couple of scalar tasks to optimize and eval in parallel
    scalarTasks = makeScalarTasks(as.data.frame(opt.path, discretes.as.factor = TRUE),
      par.set, control = control, Lambdas = Lambdas)
    # Save the weights
    weight.path = rbind(weight.path, data.frame(scalarTasks$weights, dob = loop))
    scalarTasks = scalarTasks$tasks
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
    if(control$impute.errors)
      mapply(addOptPathEl, xs, ys, error.message = evals$error.messages,
        MoreArgs = list(op = opt.path, dob = loop))
    else
      mapply(addOptPathEl, xs, ys, MoreArgs = list(op = opt.path, dob = loop))
    if (loop %in% control$save.on.disk.at) {
      save(list = c("opt.path", "fun", "par.set", "learner", "control", "show.info", "more.args"),
        file = control$save.file.path)
    }
  }

  front.index = getOptPathParetoFront(opt.path, index = TRUE)
  pareto.front = getOptPathParetoFront(opt.path, index = FALSE)

  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])

  makeS3Obj("ParEGOResult",
    pareto.front = pareto.front,
    opt.path = opt.path,
    weight.path = weight.path
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


# small helper: calculate all vectors of length k with sum n
combWithSum <- function(n, k) {
  stopifnot(k > 0L)
  
  REC <- function(n, k) {
    if (k == 1L) list(n) else
      unlist(lapply(0:n, function(i)Map(c, i, REC(n - i, k - 1L))),
        recursive = FALSE)
  }
  
  matrix(unlist(REC(n, k)), ncol = k, byrow = TRUE)
}