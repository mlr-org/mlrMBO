#  Optimizes a multicrit optimization problem with the parego algorithm

mboParEGO = function(fun, par.set, design = NULL, learner, control, show.info = TRUE, more.args = list()) {
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # shortcut names
  ninit = control$init.design.points
  crit = control$infill.crit

  opt.path = makeMBOOptPath(par.set, control)

  # helper to get extras-list for opt.path logging
  getExtras = function(crit.vals, model.fail, wmat) {
    n = length(crit.vals)
    exs = vector("list", n)
    for (i in 1:n) {
      ex = list(crit.vals[i], .model.fail = model.fail)
      names(ex)[1] = crit
      w = setNames(as.list(wmat[i, ]), paste0(".weight", 1:ncol(wmat)))
      exs[[i]] = c(ex, w)
    }
    return(exs)
  }

  # Calculate all possible weight vectors and save them
  all.possible.weights = combWithSum(control$parego.s, control$number.of.targets) / control$parego.s
  # rearrange them a bit - we want to have the margin weights on top of the matrix
  # tricky: all margin weights have maximal variance
  vars = apply(all.possible.weights, 1, var)
  all.possible.weights = rbind(diag(control$number.of.targets), all.possible.weights[!vars == max(vars),])

  wmat = matrix(NA, nrow = ninit, ncol = control$number.of.targets)
  generateMBODesign(design, fun, par.set, opt.path, control, show.info, oldopts, more.args,
    extras = getExtras(rep(NA, ninit), model.fail = NA_character_, wmat))

  # new control object for the scalar soo iteration, we always minimize here
  ctrl2 = control
  ctrl2$minimize = TRUE

  saveStateOnDisk(0L, control, fun, learner, par.set, opt.path, control, show.info, more.args)

  # do the mbo magic
  # if we are restarting from a save file, we possibly start in a higher iteration
  start.loop = max(getOptPathDOB(opt.path)) + 1
  for (loop in start.loop:control$iters) {
    # scalarize + train + propose
    scalar = makeScalarTasks(par.set, opt.path, control, all.possible.weights)
    models = lapply(scalar$tasks, train, learner = learner)
    props = lapply(models, proposePoints, par.set = par.set,
      control = ctrl2, opt.path = opt.path)
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE))

    extras = getExtras(
      crit.vals = extractSubList(props, "crit.vals"),
      model.fail = extractSubList(props, "model.fail"),
      wmat = scalar$weights
    )

    evalProposedPoints(loop, prop.points, par.set, opt.path, control,
      fun, learner, show.info, oldopts, more.args, extras)
  }

  pareto.inds = getOptPathParetoFront(opt.path, index = TRUE)

  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  makeS3Obj(c("MBOMultiObjResult", "MBOResult"),
    pareto.front = getOptPathY(opt.path)[pareto.inds, ],
    pareto.set = lapply(pareto.inds, function(i) getOptPathEl(opt.path, i)$x),
    opt.path = opt.path
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
