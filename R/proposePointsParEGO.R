proposePointsParEGO = function(models, par.set, control, opt.path, iter, weight.mat) {
  control2 = control;
  control2$propose.points = 1L
  control2$number.of.targets = 1L
  control2$minimize = TRUE
  props = list()

  #FIXME: could be done in parallel
  for (npoints in 1:control$propose.points) {
    props[[npoints]] = proposePointsByInfillOptimization(models, par.set, control2, opt.path, iter, models.unlist = TRUE)
  }

  res = list(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE))
  )
  res$weight.mat = weight.mat
  return(res)
}
