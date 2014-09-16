# some functions call proposePointByInfillOptimization multiple times
# this helpers joins these results to a single result
joinProposedPoints = function(props) {
  list(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE))
  )
}

# generate a few random points if ANY model failed
checkFailedModels = function(models, npoints) {
  isfail = vlapply(models, isFailureModel)
  prop = NULL
  if (any(isfail)) {
    # if error in any model, return first msg
    prop.points = generateDesign(npoints, par.set, randomLHS)
    prop$prop.points = convertDataFrameCols(prop.points, ints.as.num = TRUE, logicals.as.factor = TRUE)
    prop$crit.vals = matrix(rep(NA_real_, npoints), ncol = 1L)
    prop$errors.model = getFailureModelMsg(models[[which.first(isfail)]])
  }
  return(list(ok = all(!isfail), prop = prop))
}


