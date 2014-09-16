
# Use LCB single crit but sample multiple different lambdas
# NOTE THAT WE MIGHT PRODUCE LESS POINS THERN REQUESTED IF SOME OF THEM ARE EXTREMELY CLOSE
# FIXME: document this. also maybe improve?
proposePointsParallelLCB = function(models, par.set, control, opt.path, iter, ...) {
  # copy control and optimize multiple times with singlecrit lcb / different lambda
  control2 = control; control2$propose.points = 1L; control2$infill.crit = "lcb"
  # draw lambdas from exp dist
  lambdas = rexp(control$propose.points)
  props = list()

  #FIXME: could be done in parallel
  for (npoints in 1:control$propose.points) {
    control2$infill.crit.lcb.lambda = lambdas[npoints]
    props[[npoints]] = proposePointsByInfillOptimization(models, par.set, control2, opt.path, iter, models.unlist = TRUE, ...)
  }

  # newdes = setAttribute(newdes, "multipoint.lcb.lambdas", lambdas)
  list(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE))
  )
}

