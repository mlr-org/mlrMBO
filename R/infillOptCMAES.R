# Optimizers for infill criteria

# General interface
#
# @param infill.crit [\code{function}]\cr
#   Infill criterion function.
# @param models [\code{\link{WrappedModel}}]\cr
#   Model fitted on designs.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @param designs [list of \code{data.frame}]\cr
#   List with usually one element containing the Design of already visited points.
# @param iter [\integer(1)]
#   Current iteration
# @param ... [\code{ANY}]\cr
#   Additional arguments passed to \code{infill.crit}.
# @return [\code{data.frame}]. One proposed point that should be evaluated.

#FIXME we need other optimizers for mixed, depenent param spaces. dont forget refactorNAS then

# the first start is always at the best point of the current opt.path.
# works only for numerics and integers, latter are simply rounded.
infillOptCMAES = function(infill.crit, models, control, par.set, opt.path, designs, iter, ...) {
  rep.pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  cmaes.control = control$infill.opt.cmaes.control

  fn = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      newdata = as.data.frame(t(x))
      colnames(newdata) = rep.pids
      infill.crit(newdata, models, control, par.set, designs, iter, ...)
    },
    par.set = par.set,
    vectorized = TRUE
  )

  cmaes.control = insert(list(stop.ons = c(cmaesr::getDefaultStoppingConditions())), cmaes.control)

  cmaes.control = insert(list(
      restart.triggers = extractSubList(cmaes.control$stop.ons, "code")), cmaes.control)

  # set number of restarts
  cmaes.control = insert(cmaes.control, list(max.restarts = control$infill.opt.restarts))

  # select first start point as currently best
  if (control$n.objectives == 1L) {
    start.point = unlist(getOptPathEl(opt.path, getOptPathBestIndex(opt.path))$x)
    result = cmaesr::cmaes(fn, start.point = start.point, monitor = NULL, control = cmaes.control)
  } else {
    # FIXME: Intelligent start point for multi crit
    # Note: cmaesr can only optimize single crit. But we might call cmaes to optimize single component.
    result = cmaesr::cmaes(fn, monitor = NULL, control = cmaes.control)
  }

  # all CMA-ES runs failed. Therefore we sample a random point and warn
  if (is.infinite(result$best.fitness)) {
    warningf("Infill optimizer CMA-ES crashed. Random point generated instead.")
    res = t(sampleValue(par.set))
  } else {
    res = t(result$best.param)
  }
  setColNames(as.data.frame(res), rep.pids)
}
