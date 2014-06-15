#  Optimizes a multicrit optimization problem with the parego algorithm

mboParEGO = function(fun, par.set, design = NULL, learner, control, show.info = TRUE,
  more.args = list(), continue = NULL) {

  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # shortcut names
  ninit = if(is.null(design)) control$init.design.points else nrow(design)
  crit = control$infill.crit

  # helper to get extras-list for opt.path logging
  getExtras = function(crit.vals, model.fail, wmat) {
    n = length(crit.vals)
    exs = vector("list", n)
    for (i in 1:n) {
      ex = list(crit.vals[i], .model.fail = model.fail[i])
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

  # for normal start, we setup initial design, otherwise take stuff from continue object from disk
  if (is.null(continue)) {
    opt.path = makeMBOOptPath(par.set, control)
    wmat = matrix(NA, nrow = ninit, ncol = control$number.of.targets)
    generateMBODesign(design, fun, par.set, opt.path, control, show.info, oldopts, more.args,
      extras = getExtras(rep(NA, ninit), model.fail = NA_character_, wmat))
    models = namedList(control$save.model.at)
    saveStateOnDisk(0L, fun, learner, par.set, opt.path, control, show.info, more.args, models, NULL, NULL)
  } else {
    if (!is.null(continue$mbo.result)) {
      warningf("mboContinue: No need to continue, we were already finished. Simply returning stored result.")
      return(mbo.result)
    }
    opt.path = continue$opt.path
    models = continue$models
  }

  # new control for scalar soo iteration, always minimize and propose 1 point
  ctrl2 = control
  ctrl2$propose.points = 1L
  ctrl2$minimize = TRUE

  # if we are restarting from a save file, we possibly start in a higher iteration
  loop = max(getOptPathDOB(opt.path)) + 1
  while (loop <= control$iters) {
    # scalarize + train + propose
    scalar = makeScalarTasks(par.set, opt.path, control, all.possible.weights)
    newmods = lapply(scalar$tasks, train, learner = learner)
    if (loop %in% control$save.model.at)
      models[[as.character(loop)]] = newmods
    props = lapply(newmods, proposePoints, par.set = par.set,
      control = ctrl2, opt.path = opt.path)
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE))

    extras = getExtras(
      crit.vals = extractSubList(props, "crit.vals"),
      model.fail = extractSubList(props, "model.fail"),
      wmat = scalar$weights
    )
    evalProposedPoints(loop, prop.points, par.set, opt.path, control,
      fun, learner, show.info, oldopts, more.args, extras)
    saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args, models, NULL, NULL)
    loop = loop + 1L
  }

  pareto.inds = getOptPathParetoFront(opt.path, index = TRUE)

  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  res = makeS3Obj(c("MBOMultiObjResult", "MBOResult"),
    pareto.front = getOptPathY(opt.path)[pareto.inds, ],
    pareto.set = lapply(pareto.inds, function(i) getOptPathEl(opt.path, i)$x),
    opt.path = opt.path,
    models = models
  )

  # make sure to save final res on disk
  saveStateOnDisk(loop, fun, learner, par.set, opt.path, control, show.info, more.args,
    models, NULL, res)

  return(res)
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
