# FIXME:
# a) check in general that maximization of y works. check in plot and also with other MCO methods

proposePointsDIB = function(models, par.set, control, opt.path, iter) {
  if (control$propose.points > 1L) {
    # draw lambdas from exp dist + create 1 control for each for single crit with lambda-LCB
    z = createRandomLCBControls(control, "dib")

    # now sample multiple refpoints between nadir and normal refpoint
    y = getOptPathY(opt.path)
    mults = ifelse(control$minimize, 1, -1)
    y2 = y %*% diag(mults)
    corner1 = apply(y2, 2, min)
    corner2 = apply(y2, 2, max)
    ps.cube = makeNumericParamSet(lower = corner1, upper = corner2)
    ref.points = as.matrix(generateDesign(control$propose.points - 1L, ps.cube))
    # always use max(y_i) as the first refpoint
    ref.points = rbind(corner2, ref.points)
    # set new refpoints in controls
    for (i in seq_row(ref.points)) {
      z$controls[[i]]$multicrit.ref.point.method = "const"
      z$controls[[i]]$multicrit.ref.point.val = as.numeric(ref.points[i,])
    }

    # propose the points
    props = parallelMap(proposePointsByInfillOptimization, control = z$controls,
      more.args = list(models = models, par.set = par.set, opt.path = opt.path, iter = iter))
    res = joinProposedPoints(props)
    # store extra info
    res$multipoint.lcb.lambdas = z$lambdas
    res$multicrit.ref.points = ref.points
  } else {
    # dont do the fancy stuff if we just propose 1 point
    res = proposePointsByInfillOptimization(models, par.set, control, opt.path, iter)
  }
  return(res)
}


