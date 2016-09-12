# Helper function to query the model for jobs with a runtime below given t.max
#
# @param t.max [\code{numeric}] \cr
#   the Maximum Time for the Random Jobs
# @param  empty.nodes [\code{numeric}] \cr
#   the Number of Jobs to return
# @param opt.state [\code{OptState}]\cr
# @return [\code{list}] \cr
#   List containing the xs, xs.trafo and extras for the choosen Jobs


fillRandom = function(t.max, empty.slots ,opt.state) {
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  control2 = control
  par.set = getOptProblemParSet(getOptStateOptProblem(opt.state))
  prop = proposePointsRandom2(par.set = par.set, n = empty.slots * 50)
  control2$infill.crit = "random"
  control2$propose.points = empty.slots * 50
  time.model = getOptStateTimeModel(opt.state)
  time.prediction = predict(time.model, newdata = prop$prop.points)
  predicted.time = getPredictionResponse(time.prediction)
  #partly taken from evalProposedPoints.OptState
  extras2 = getExtras(
    n = nrow(prop$prop.points),
    prop = c(prop, list(
      predicted.time = predicted.time,
      predicted.time.se = getPredictionSE(time.prediction))),
    train.time = NA_real_,
    control = control2)
  for (i in seq_along(extras2)) {
    empty = getExtras(1, control = control, prop = NULL, train.time = NA_real_)[[1]]
    extras2[[i]] = insert(empty, extras2[[i]], intersect(names(empty), names(extras2[[i]])))
  }
  xs2 = dfRowsToList(prop$prop.points, par.set)
  xs2 = lapply(xs2, repairPoint, par.set = par.set)
  #narrow down to fit inside of t.max
  inds = which(predicted.time < t.max)
  #narrow down to one job for each free node
  inds = head(inds, empty.slots)
  xs2.trafo = lapply(xs2[inds], trafoValue, par = getOptProblemParSet(getOptStateOptProblem(opt.state)))

  list(xs2[inds], xs2.trafo, extras2[inds])
}