# to list + repair + eval + save state
#
evalProposedPoints = function(loop, prop.points, par.set, opt.path, control, 
  fun, learner, show.info, oldopts, more.args, extras) {

  xs = dfRowsToList(prop.points, par.set)
  xs = lapply(xs, repairPoint, par.set = par.set)
  evalTargetFun(fun, par.set, loop, xs, opt.path, control, show.info, oldopts, more.args, extras)
  saveStateOnDisk(loop, control, fun, learner, par.set, opt.path, control, show.info, more.args)
}
