#FIXME: retrain kriging faster

#FIXME: how to choose best element. with noise? without?
#FIXME: different name for final evals in output (not last step number)

#FIXME: cmaes doesn't work when optimum in constraints

#  Optimizes a function with sequential model based optimization.
#
# Input Params are the same as for the main mbo function, except for design.
# In order to make an nice restart solution possible, design can also be a
# opt.path here.
#
# @return [\code{list}]:
#   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x}, either from evals during optimization or from requested final evaluations, if those were greater than 0.}
#   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#   \item{times [\code{numeric}]}{Vector of times it took to evaluate the objective.}
#   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#   \item{multipoint.lcb.lambdas [\code{matrix(iters, proposed.points)}]}{Sampled lambda values for multipoint lcb method.}
mboSingleObj = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, more.args=list()) {
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # get parameter ids repeated length-times and appended number
  y.name = control$y.name

  # generate initial design
  mbo.design = generateMBODesign(design, fun, par.set, control, show.info, oldopts, more.args)
  design = cbind(mbo.design$design.x, setColNames(mbo.design$design.y, y.name))
  opt.path = mbo.design$opt.path
  opt.path2 = mbo.design$opt.path2
  times = mbo.design$times
  # we now have design.y and design

  # set up initial mbo task
  rt = makeMBOTask(design, par.set, control)
  model = train(learner, rt)

  models = namedList(control$save.model.at)
  res.vals = namedList(control$resample.at)

  if (0L %in% control$save.model.at) {
    models[["0"]] = model
  }

  if (0L %in% control$resample.model.at) {
    r = resample(learner, rt, control$resample.desc, measures=control$resample.measures)
    res.vals[["0"]] = r$aggr
  }

  # Save on disk?
  if (0 %in% control$save.on.disk.at) {
    save(list = c("opt.path", "fun", "par.set", "learner", "control", "show.info", "more.args"),
      file = control$save.file.path)
  }

  # store sampled lambdas for this special method in return val
  multipoint.lcb.lambdas = if (control$multipoint.method == "lcb")
    matrix(nrow=0, ncol=control$propose.points)
  else
    NULL

  # do the mbo magic
  # if we are restarting from a save file, we possibly start in a higher iteration
  start.loop = max(getOptPathDOB(opt.path)) + 1
  for (loop in start.loop:control$iters) {

    # propose new points and evaluate target function
    prop.design = proposePoints(model, par.set, control, opt.path)
    prop.points = prop.design$prop.points
    prop.points.crit.values = prop.design$prop.points.crit.values

    # handle lambdas for this method
    if (control$multipoint.method == "lcb") {
      multipoint.lcb.lambdas = rbind(multipoint.lcb.lambdas, attr(prop.points, "multipoint.lcb.lambdas"))
      attr(prop.points, "multipoint.lcb.lambda") =  NULL
    }

    xs = dfRowsToList(prop.points, par.set)
    xs = lapply(xs, repairPoint, par.set = par.set)
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, more.args)
    times = c(times, evals$times)

    # update optim trace and model
    Map(function(x, y, err) addOptPathEl(opt.path, x = x, y = y, dob = loop, error.message = err),
      xs, evals$ys, evals$error.messages)
    Map(function(x, y, cv) addOptPathEl(opt.path2, x = x, y = c(y, cv), dob = loop), xs, evals$ys, prop.points.crit.values)
    rt = makeMBOTask(as.data.frame(opt.path, discretes.as.factor = TRUE), par.set, control)
    model = train(learner, rt)
    if (loop %in% control$save.model.at)
      models[[as.character(loop)]] = model
    if (loop %in% control$resample.at) {
      r = resample(learner, rt, control$resample.desc, measures = control$resample.measures)
      res.vals[[as.character(loop)]] = r$aggr
    }
    # Save on disk?
    if (loop %in% control$save.on.disk.at) {
      save(list = c("opt.path", "fun", "par.set", "learner", "control", "show.info", "more.args"),
        file = control$save.file.path)
    }
  }

  design = getTaskData(rt, target.extra=TRUE)$data
  final.index = chooseFinalPoint(fun, par.set, model, opt.path, y.name, control)
  best = getOptPathEl(opt.path, final.index)
  x = best$x
  y = best$y

  if (control$final.evals > 0L) {
    if (show.info)
      messagef("Performing %i final evals", control$final.evals)
    # do some final evaluations and compute mean of target fun values
    xs = replicate(control$final.evals, best$x, simplify=FALSE)
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, more.args)
    ys = evals$ys
    times = c(times, evals$times)
    best$y = mean(ys)
  }
  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])

  # make sure to strip name of y
  makeS3Obj(c("MBOSingleObjResult", "MBOResult"),
    x = best$x,
    # strip name
    y = as.numeric(best$y),
    opt.path = opt.path,
    opt.path2 = opt.path2,
    times = times,
    resample = res.vals,
    models = models,
    multipoint.lcb.lambdas = multipoint.lcb.lambdas
  )
}

#' @export
print.MBOResult = function(x, ...) {
  op = x$opt.path
  catf("Recommended parameters:")
  catf(paramValueToString(op$par.set, x$x))
  catf("Objective: %s = %.3f\n", op$y.names[1], x$y)
  catf("Optimization path")
  n1 = sum(op$env$dob == 0)
  n2 = length(op$env$dob) - n1
  catf("%i + %i entries in total, displaying last 10 (or less):", n1, n2)
  print(tail(as.data.frame(op), 10))
}
