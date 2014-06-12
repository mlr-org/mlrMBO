#  Optimizes a multicrit optimization problem  with sequential model based
#  optimization using the parego algorithm
#
# Input Params are the same as for the main mbo function, except for design.
# In order to make an nice restart solution possible, design can also be a
# opt.path here.
#
# @return [\code{list}]:
#   \item{pareto.front [\code{matrix}]}{Pareto Front of all evaluated points.}
#   \item{pareto.set [\code{list(list)}]}{List of named list of pareto set.}
#   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#   \item{weight.path [\code{data.frame}]{Data.frame containing all used weights.}
mboParego = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, more.args=list()) {
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # Calculate all possible weight vectors and save them
  all.possible.weights = combWithSum(control$parego.s, control$number.of.targets) / control$parego.s
  # rearrange them a bit - we want to have the margin weights on top of the matrix
  # tricky: all margin weights have maximal varianz
  vars = apply(all.possible.weights, 1, var)
  all.possible.weights = rbind(diag(control$number.of.targets), all.possible.weights[!vars == max(vars),])

  # generate initial design
  mbo.design = generateMBODesign(design, fun, par.set, control, show.info, oldopts, more.args)
  design = cbind(mbo.design$design.x, mbo.design$design.y)
  opt.path = mbo.design$opt.path
  times = mbo.design$times
  y.name = control$y.name
  # we now have design.y and design
  
  
  # new control object for the skalar single obj. optimization
  ctrl2 = control
  ctrl2$minimize = TRUE
  
  # Save on disk?
  if (0 %in% control$save.on.disk.at) {
    save(list = c("opt.path", "fun", "par.set", "learner", "control", "show.info", "more.args"),
      file = control$save.file.path)
  }

  # initialize data.frame to save the used weights
  # FIXME: We want to save this information in the opt.path!
  weight.path = data.frame()

  # do the mbo magic
  # if we are restarting from a save file, we possibly start in a higher iteration
  start.loop = max(getOptPathDOB(opt.path)) + 1
  for (loop in start.loop:control$iters) {
    # create a couple of scalar tasks to optimize and eval in parallel
    scalar.tasks = makeScalarTasks(as.data.frame(opt.path, discretes.as.factor = TRUE),
      par.set, control = control, all.possible.weights = all.possible.weights)
    # Save the weights
    weight.path = rbind(weight.path, data.frame(scalar.tasks$weights, dob = loop))
    scalar.tasks = scalar.tasks$tasks
    models = lapply(scalar.tasks, train, learner = learner)
    # propose new points and evaluate target function
    prop.designs = lapply(models, proposePoints, par.set = par.set,
      control = ctrl2, opt.path = opt.path)
    prop.designs = do.call(rbind, lapply(prop.designs, function(x) x$prop.points))
    xs = dfRowsToList(prop.designs, par.set)
    xs = lapply(xs, repairPoint, par.set = par.set)
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, more.args)
    ys = convertRowsToList(evals$ys)
    # store in opt path
    mapply(addOptPathEl, xs, ys, error.message = evals$error.messages,
      MoreArgs = list(op = opt.path, dob = loop))
    # Save on disk?
    if (loop %in% control$save.on.disk.at) {
      save(list = c("opt.path", "fun", "par.set", "learner", "control", "show.info", "more.args"),
        file = control$save.file.path)
    }
  }

  pareto.inds = getOptPathParetoFront(opt.path, index = TRUE)
  pareto.front = getOptPathY(opt.path)[pareto.inds, ]
  pareto.set = lapply(pareto.inds, function(i) getOptPathEl(opt.path, i)$x)


  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])

  makeS3Obj(c("MBOMultiObjResult", "MBOResult"),
    pareto.front = pareto.front,
    pareto.set = pareto.set,
    opt.path = opt.path,
    weight.path = weight.path
  )
}

#' @export
print.MBOMultiObjResult = function(x, ...) {
  print(x$pareto.front)
  print(tail(as.data.frame(x$opt.path), 10))
}


# small helper: calculate all integer vectors of length k with sum n
combWithSum = function(n, k) {
  fun = function(n, k) {
    if (k == 1L)
      list(n)
    else
      unlist(lapply(0:n, function(i) Map(c, i, fun(n - i, k - 1L))),
        recursive = FALSE)
  }
  matrix(unlist(fun(n, k)), ncol = k, byrow = TRUE)
}
