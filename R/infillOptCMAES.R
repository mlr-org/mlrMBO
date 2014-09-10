# Optimizers for infill criteria

# General interface
#
# @param infill.crit [\code{function}]\cr
#   Infill criterion function.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @param iter [\integer(1)]
#   Current iteration
# @param ... [\code{ANY}]\cr
#   Additional arguments passed to \code{infill.crit}.
# @return [\code{data.frame}]. One proposed point that should be evaluated.

#FIXME we need other optimizers for mixed, depenent param spaces. dont forget refactorNAS then

# cmaes with simple random restarts.
# the first start is always at the best point of the current opt.path.
# works only for numerics and integers, latter are simply rounded.
infillOptCMAES = function(infill.crit, model, control, par.set, opt.path, design, iter, ...) {
  # extract lower and upper bound for params
  low = getLower(par.set)
  upp = getUpper(par.set)

  rep.pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  #eval all points of 1 generation at once
  cmaes.control = control$cmaes.control
  cmaes.control$vectorized = TRUE
  f = function(x) {
    newdata = as.data.frame(t(x))
    colnames(newdata) = rep.pids
    infill.crit(newdata, model, control, par.set, design, iter, ...)
  }

  results = vector("list", control$infill.opt.restarts)
  # restart optimizer, first start point is currently best
  for (i in 1:control$infill.opt.restarts) {
    if (i == 1) {
      start = getOptPathEl(opt.path, getOptPathBestIndex(opt.path))$x
    } else {
      start = sampleValue(par.set)
    }
    start = unlist(start)
    results[[i]] = cma_es(par = start, fn = f, lower = low, upper = upp, control = control$infill.opt.cmaes.control)
  }
  ys = extractSubList(results, "value")
  ys = ys[!is.infinite(ys)]
  res = NULL
  # all CMA-ES runs failed. Therefore we sample a random point and warn
  if (length(ys) == 0) {
    res = t(sampleValue(par.set))
    warningf("Infill optimizer CMA-ES crashed %i times. Random point generated instead.", control$infill.opt.restart)
  } else {
    j = which(rank(ys, ties.method = "random") == 1L)
    res = t(results[[j]]$par)
  }
  setColNames(as.data.frame(res), rep.pids)
}

# FIXME: allow DiceOptim optimizer later...
# infillOptEI = function(infill.crit, model, control, par.set, opt.path, ...) {
#   # extract lower and upper bound for params
#   low = getLower(par.set)
#   upp = getUpper(par.set)
#
#   i = getOptPathBestIndex(opt.path, ties = "random")
#   start = unlist(getOptPathEl(opt.path, i)$x)
#   capture.output(design <- max_EI(model$learner.model,
#     lower = low, upper = upp, parinit = start)$par)
#   as.data.frame(design)
# }

